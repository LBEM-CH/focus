# Python utilities for Focus
# Author: Ricardo Righetto
# E-mail: ricardo.righetto@unibas.ch
# http://www.focus-em.org

import numpy as np
import focus_ctf

# # # NOTE ON FFT # # #
# # At some point I might replace np.fft with pyfftw, but I couldn't get it working properly yet (see below).
# # And, I don't go for scipy.fftpack because the output of scipy.fftpack.rfft is weird, plus I/O datatype manipulation can be cumbersome compared to np.fft. 

# try:

# 	# http://pyfftw.readthedocs.io/en/latest/source/tutorial.html#interfaces-tutorial
# 	import pyfftw

# 	# Monkey patch numpy_fft with pyfftw.interfaces.numpy_fft
# 	np.fft = pyfftw.interfaces.numpy_fft
# 	# np.empty = pyfftw.empty_aligned

# 	# Turn on the cache for optimum performance
# 	pyfftw.interfaces.cache.enable()
# 	pyfftw.interfaces.cache.set_keepalive_time(60)

# except ImportError:

# 	print( "PyFFTW not found. Falling back to numpy.fft (slow).\nYou may want to install PyFFTW by running:\n'pip install pyfftw'" )

# print np.fft.__file__


def RadialIndices( imsize = [100, 100], rounding=True, normalize=False, rfft=False, xyz=[0,0,0], nozero=True ):
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

		raise ValueError( "Object should have 2 or 3 dimensions: len(imsize) = %d " % len(imsize))

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

	if nozero:

		rmesh[rmesh == 0] = 1e-3 # Replaces the "zero radius" by a small value to prevent division by zero in other programs

	return rmesh, np.nan_to_num( amesh )

def Shift( img, shift = [0,0,0] ):
# Shifts a 3D volume by phase shifting in Fourier space
# Compatible with relion_image_handler
# The shift to be applied is given by the array 'shift'
# By default employs rfft for speedup.

	imsize = np.array( img.shape )
	shift = np.array( shift ).astype( 'float' )

	if len( imsize ) != len( shift ):

		raise ValueError( "Shift dimensions do not match image/volume dimensions: len(img.shape) = %d and len(shift) = %d " % len( imsize ) )

	if len( imsize ) > 3:

		raise ValueError( "Object should have 2 or 3 dimensions: len(img.shape) = %d " % len( imsize)  )

	shift = np.flipud( shift )

	m = np.mod(imsize, 2) # Check if dimensions are odd or even

	if len(imsize) == 2:


		[xmesh, ymesh] = np.mgrid[-imsize[0]//2+m[0]:(imsize[0]-1)//2+1, 0:imsize[1]//2+1]
		xmesh = np.fft.ifftshift( xmesh )

	else:

		[xmesh, ymesh, zmesh] = np.mgrid[-imsize[0]//2+m[0]:(imsize[0]-1)//2+1, -imsize[1]//2+m[1]:(imsize[1]-1)//2+1, 0:imsize[2]//2+1]
		xmesh = np.fft.ifftshift( xmesh )
		ymesh = np.fft.ifftshift( ymesh )

	ft = np.fft.rfftn( img )

	if len( imsize ) == 2:

		ft_shift = np.exp( -2.0 * np.pi * 1j * ( shift[0] * xmesh / imsize[0] + shift[1] * ymesh / imsize[1] ) )

	else:

		ft_shift = np.exp( -2.0 * np.pi * 1j * ( shift[0] * xmesh / imsize[0] + shift[1] * ymesh / imsize[1] + shift[2] * zmesh / imsize[2] ) )


	return np.fft.irfftn( ft * ft_shift , s=img.shape )


# def Rotate( img, rot = [0,0,0], interpolation='trilinear', pad=1, do_sinc=True ):
def Rotate( img, rot = [0,0,0], interpolation='trilinear', pad=1 ):
# Rotates a 2D image or 3D volume
# Rotation array 'rot' is given in SPIDER conventions (ZYZ rotation): PHI, THETA, PSI - same as in relion_rotate
# Interpolation can be 'nearest' (fast but poor), 'trilinear' (slower but better) or 'cosine' (slightly slower than trilinear and maybe slightly worse - not clear)
# 'pad' is the padding factor to multiply the input dimensions. Better results can be achieved by resampling the input image beforehand (padding the Fourier transform), but that may be very RAM-demanding. See function Resample().
# do_sinc is to pre-correct the input data by deconvolving the interpolant
# For detailed definitions please see Baldwin & Penczek, JSB 2007.

	if np.isscalar( rot ):

		rot = [rot]

	imsize_ori = np.array( img.shape )

	# if do_sinc:

	# 	rmesh = RadialIndices( img.shape, rounding=False, normalize=True, rfft=True )[0]

	# 	sinc = np.sinc( rmesh )

	# 	if interpolation == 'nearest':

	# 		img = np.fft.irfftn( np.fft.rfftn( img ) / sinc )

	# 	elif interpolation == 'trilinear':

	# 		img = np.fft.irfftn( np.fft.rfftn( img ) / ( sinc * sinc ) )

	rot = np.array( rot ).astype( 'float' ) * np.pi / 180.0

	if len( imsize_ori ) == 3:

		if len( imsize_ori ) != len( rot ):

			raise ValueError( "Rotation dimensions do not match image/volume dimensions: len(img.shape) = %d and len(rot) = %d " % ( len( imsize ), len( rot ) ) )

		if imsize_ori[0] == 1:

			img = img.reshape( ( imsize_ori[1], imsize_ori[2] ) )

	elif len( imsize_ori ) == 2 and len ( rot ) != 1:

			raise ValueError( "Rotation dimensions do not match image/volume dimensions: len(img.shape) = %d and len(rot) = %d " % ( len( imsize ), len( rot ) ) )

	if len( imsize_ori ) > 3:

		raise ValueError( "Object should have 2 or 3 dimensions: len(img.shape) = %d " % len( imsize)  )

	imsize = np.array( img.shape ) * pad
	if pad != 1:

		img = Resample( img, newsize=imsize )

	m = np.mod(imsize, 2) # Check if dimensions are odd or even

	if len(imsize) == 2:

		[xmesh, ymesh] = np.mgrid[-imsize[0]//2+m[0]:(imsize[0]-1)//2+1, -imsize[1]//2+m[1]:(imsize[1]-1)//2+1]
		psi = rot[0]

		rotmat = np.matrix( [[np.cos( psi ), -np.sin( psi )], [np.sin( psi ), np.cos( psi )]] )

		# sqrt2 = np.sqrt( 2 )
		sqrt2 = 2

		ymeshrot = ( sqrt2 * imsize[1] - imsize[1] )//2 + ymesh * rotmat[0,0] + xmesh * rotmat[1,0] + imsize[1]//2 - m[1]
		xmeshrot = ( sqrt2 * imsize[0] - imsize[0] )//2 + ymesh * rotmat[0,1] + xmesh * rotmat[1,1] + imsize[0]//2 - m[0]

		img2 = Resize( img, newsize=imsize * sqrt2 + 1 )
		del img

		rotimg = np.zeros( img2.shape )

		if interpolation == 'nearest':

			rotimg = img2[ np.round( xmeshrot ).astype('int'), np.round( ymeshrot ).astype('int') ]

		else:

			x0 = np.floor( xmeshrot ).astype( 'int' )
			x1 = np.ceil( xmeshrot ).astype( 'int' )
			y0 = np.floor( ymeshrot ).astype( 'int' )
			y1 = np.ceil( ymeshrot ).astype( 'int' )

			# import warnings
			# with warnings.catch_warnings():
			# 	warnings.filterwarnings( "ignore", category=RuntimeWarning )

			# 	xd = np.nan_to_num( ( xmeshrot - x0 ) / ( x1 - x0 ) )
			# 	yd = np.nan_to_num( ( ymeshrot - y0 ) / ( y1 - y0 ) )

			xd = xmeshrot - x0
			yd = ymeshrot - y0

			if interpolation == 'cosine': # Smoother than trilinear at negligible extra computation cost?

				xd = ( 1 - np.cos( xd * np.pi) ) / 2
				yd = ( 1 - np.cos( yd * np.pi) ) / 2

			# c00 = img2[x0, y0]
			# c01 = img2[x0, y1]
			# c10 = img2[x1, y0]
			# c11 = img2[x1, y1]

			# c0 = c00 * ( 1 - xd ) + c10 * xd
			# c1 = c01 * ( 1 - xd ) + c11 * xd

			# c = c0 * ( 1 - yd ) + c1 * yd

			# Below is the same as the commented above, but in one line:
			rotimg = ( img2[x0, y0] * ( 1 - xd ) + img2[x1, y0] * xd ) * ( 1 - yd ) + ( img2[x0, y1] * ( 1 - xd ) + img2[x1, y1] * xd ) * yd

	else:

		[xmesh, ymesh, zmesh] = np.mgrid[-imsize[0]//2+m[0]:(imsize[0]-1)//2+1, -imsize[1]//2+m[1]:(imsize[1]-1)//2+1, -imsize[2]//2+m[2]:(imsize[2]-1)//2+1]

		# print xmesh.min(),xmesh.max()
		# print ymesh.min(),ymesh.max()
		# print zmesh.min(),zmesh.max()

		phi = rot[0]
		theta = rot[1]
		psi = rot[2]

		mat1 = np.matrix( [[np.cos( psi ), np.sin( psi ), 0], [-np.sin( psi ), np.cos( psi ), 0], [0, 0, 1]] )
		mat2 = np.matrix( [[np.cos( theta ), 0, -np.sin( theta )], [0, 1, 0], [np.sin( theta ), 0, np.cos( theta )]] )
		mat3 = np.matrix( [[np.cos( phi ), np.sin( phi ), 0], [-np.sin( phi ), np.cos( phi ), 0], [0, 0, 1]] )
		rotmat = mat1 * mat2 * mat3

		# print phi,theta,rot
		# print rotmat
		# print imsize

		# Original matrix mmultiplications, without bothering about indices:
		# zmeshrot = zmesh * rotmat[0,0] + ymesh * rotmat[1,0] + xmesh * rotmat[2,0]
		# ymeshrot = zmesh * rotmat[0,1] + ymesh * rotmat[1,1] + xmesh * rotmat[2,1]
		# xmeshrot = zmesh * rotmat[0,2] + ymesh * rotmat[1,2] + xmesh * rotmat[2,2]

		sqrt3 = np.sqrt( 3 )

		zmeshrot = ( sqrt3 * imsize[2] - imsize[2] )//2 + zmesh * rotmat[0,0] + ymesh * rotmat[1,0] + xmesh * rotmat[2,0] + imsize[2]//2 - m[0]
		ymeshrot = ( sqrt3 * imsize[1] - imsize[1] )//2 + zmesh * rotmat[0,1] + ymesh * rotmat[1,1] + xmesh * rotmat[2,1] + imsize[1]//2 - m[1]
		xmeshrot = ( sqrt3 * imsize[0] - imsize[0] )//2 + zmesh * rotmat[0,2] + ymesh * rotmat[1,2] + xmesh * rotmat[2,2] + imsize[0]//2 - m[2]

		img2 = Resize( img, newsize=imsize * sqrt3 + 1 )
		del img

		# zmeshrot = ( 2 * imsize[2] - imsize[2] )//2 + zmesh * rotmat[0,0] + ymesh * rotmat[1,0] + xmesh * rotmat[2,0] + imsize[2]//2 - m[0]
		# ymeshrot = ( 2 * imsize[1] - imsize[1] )//2 + zmesh * rotmat[0,1] + ymesh * rotmat[1,1] + xmesh * rotmat[2,1] + imsize[1]//2 - m[1]
		# xmeshrot = ( 2 * imsize[0] - imsize[0] )//2 + zmesh * rotmat[0,2] + ymesh * rotmat[1,2] + xmesh * rotmat[2,2] + imsize[0]//2 - m[2]

		# img2 = Resize( img, newsize=imsize * 2 )

		# print xmeshrot.min(),xmeshrot.max()
		# print ymeshrot.min(),ymeshrot.max()
		# print zmeshrot.min(),zmeshrot.max()

		rotimg = np.zeros( img2.shape )

		if interpolation == 'nearest':

			rotimg = img2[ np.round( xmeshrot ).astype('int'), np.round( ymeshrot ).astype('int'), np.round( zmeshrot ).astype('int') ]

		else:

			x0 = np.floor( xmeshrot ).astype( 'int' )
			x1 = np.ceil( xmeshrot ).astype( 'int' )
			y0 = np.floor( ymeshrot ).astype( 'int' )
			y1 = np.ceil( ymeshrot ).astype( 'int' )
			z0 = np.floor( zmeshrot ).astype( 'int' )
			z1 = np.ceil( zmeshrot ).astype( 'int' )

			# import warnings
			# with warnings.catch_warnings():
			# 	warnings.filterwarnings( "ignore", category=RuntimeWarning )

			# 	xd = np.nan_to_num( ( xmeshrot - x0 ) / ( x1 - x0 ) )
			# 	yd = np.nan_to_num( ( ymeshrot - y0 ) / ( y1 - y0 ) )
			# 	zd = np.nan_to_num( ( zmeshrot - z0 ) / ( z1 - z0 ) )
			xd = xmeshrot - x0
			yd = ymeshrot - y0
			zd = zmeshrot - z0

			# print zd.shape,z0.shape,z1.shape
			# print zd
			# print xd.shape

			if interpolation == 'cosine': # Smoother than trilinear at negligible extra computation cost?

				xd = ( 1 - np.cos( xd * np.pi) ) / 2
				yd = ( 1 - np.cos( yd * np.pi) ) / 2
				zd = ( 1 - np.cos( zd * np.pi) ) / 2

			# c000 = img2[x0, y0, z0]
			# c001 = img2[x0, y0, z1]
			# c010 = img2[x0, y1, z0]
			# c011 = img2[x0, y1, z1]
			# c100 = img2[x1, y0, z0]
			# c101 = img2[x1, y0, z1]
			# c110 = img2[x1, y1, z0]
			# c111 = img2[x1, y1, z1]

			# c00 = c000 * ( 1 - xd ) + c100 * xd
			# c01 = c001 * ( 1 - xd ) + c101 * xd
			# c10 = c010 * ( 1 - xd ) + c110 * xd
			# c11 = c011 * ( 1 - xd ) + c111 * xd

			# c0 = c00 * ( 1 - yd ) + c10 * yd
			# c1 = c01 * ( 1 - yd ) + c11 * yd

			# c = c0 * ( 1 - zd ) + c1 * zd

			# Below is the same as the commented above, but in one line:
			rotimg = ( ( img2[x0, y0, z0] * ( 1 - xd ) + img2[x1, y0, z0] * xd ) * ( 1 - yd ) + ( img2[x0, y1, z0] * ( 1 - xd ) + img2[x1, y1, z0] * xd ) * yd ) * ( 1 - zd ) + ( ( img2[x0, y0, z1] * ( 1 - xd ) + img2[x1, y0, z1] * xd ) * ( 1 - yd ) + ( img2[x0, y1, z1] * ( 1 - xd ) + img2[x1, y1, z1] * xd ) * yd ) * zd

	del img2
	rotimg = Resize( rotimg, newsize=imsize )

	if pad != 1:

		rotimg = Resample( rotimg, newsize=imsize_ori )

	if imsize_ori[0] == 1:

			rotimg = rotimg.reshape( ( imsize_ori[0], imsize_ori[1], imsize_ori[2] ) )

	return rotimg
	# return Resample( rotimg, apix=1.0 / pad_factor, newapix=1.0 )

# def RotateFFT( img, rot = [0,0,0], interpolation='trilinear', pad=1, do_sinc=True ):
def RotateFFT( img, rot = [0,0,0], interpolation='trilinear', pad=1 ):
# Same as Rotate() function but doing the rotation in Fourier space with the required special treatments

	imsize = np.array( img.shape )

	# if do_sinc:

	# 	rmesh = RadialIndices( img.shape, rounding=False, normalize=True, rfft=False )[0]

	# 	sinc = np.sinc( rmesh )

	# 	if interpolation == 'nearest':

	# 		img /= sinc

	# 	elif interpolation == 'trilinear':

	# 		img /= ( sinc * sinc )


	imgpad = np.fft.fftshift( Resize( img, newsize=imsize * pad ) ) # Pad the real-space image, and FFT-shift the result for proper centering of the phases in subsequent operations
	del img

	F = np.fft.fftshift( np.fft.fftn( imgpad ) ) # Do the actual FFT of the input
	del imgpad
	Frot = Rotate( F, rot, interpolation=interpolation, pad=1 ) # Do the actual rotation of the FFT
	del F
	I = np.fft.ifftn( np.fft.ifftshift( Frot ) ).real # FFT-back the result to real space
	del Frot

	return Resize( np.fft.ifftshift( I ), newsize=imsize) # Undo the initial FFT-shift in real space and crop to the original size of the input

def RotationalAverage( img, nomean=False ):
# Compute the rotational average of a 2D image or 3D volume
# 'nomean' is a switch to compute the Rotational Sum instead of the Rotational Average

	rmesh = RadialIndices( img.shape, rounding=True )[0]

	rotavg = np.zeros( img.shape )

	if nomean:

		for r in np.unique( rmesh ):

			idx = rmesh == r
			rotavg[idx] = img[idx].sum()

	else:	

		for r in np.unique( rmesh ):

			idx = rmesh == r
			rotavg[idx] = img[idx].mean()

	return rotavg

def RadialProfile( img, amps=False ):
# Compute the 1D radial profile of a 2D image or 3D volume:
# 'amps' is a flag to tell whether we want a radial profile of the Fourier amplitudes

	orgshape = img.shape

	if amps:

		# img = np.abs( np.fft.fftshift( np.fft.fftn( img ) ) )
		img = np.abs( np.fft.rfftn( img, norm='ortho' ) ) # Normalization necessary to ensure same "scale" as real-space input
		rfft = True

	else:

		rfft = False


	rmesh = RadialIndices( orgshape, rounding=True, rfft=rfft )[0].ravel()
	# print img.shape,rmesh.shape

	# r_unique = np.unique( rmesh )
	# profile = np.zeros( len( r_unique ) )

	# for j,r in enumerate( np.unique( r_unique ) ):

	# 	idx = rmesh == r
	# 	profile[j] = img[idx].mean()

	# The above works, but the formulation below is much faster:
	profile = np.bincount( rmesh, img.ravel() ) / np.bincount( rmesh )

	return profile

def RadialFilter( img, filt, return_filter = False ):
# Given a list of factors 'filt', radially multiplies the Fourier Transform of 'img' by the corresponding term in 'filt'

	rmesh = RadialIndices( img.shape, rounding=True, rfft=True )[0]

	ft = np.fft.rfftn( img )
	# print len(np.unique( rmesh )),len(filt)
	# j = 0
	for j,r in enumerate( np.unique( rmesh ) ):

		idx = rmesh == r
		ft[idx] *= filt[j]
		# j += 1

	if return_filter:

		filter2d = np.zeros( rmesh.shape )
		# j = 0
		for j,r in enumerate( np.unique( rmesh ) ):

			idx = rmesh == r
			filter2d[idx] = filt[j]
			# j += 1

	if not return_filter:

		return np.fft.irfftn( ft, s=img.shape )

	else:

		return np.fft.irfftn( ft, s=img.shape ), filter2d

def MatchPowerSpectra( img1, img2 ):
# Will make the radial power spectrum (PS) of img1 equal to that of img2

	PSprofile1 = RadialProfile( img1, amps=True )
	PSprofile2 = RadialProfile( img2, amps=True )

	return RadialFilter( img1, PSprofile2/Profile1, return_filter = False )

def MatchAmplitudes( img1, img2 ):
# Will make the amplitudes of the Fourier transform of img1 equal to those of img2

	phi1 = np.angle( np.fft.rfftn( img1 ) )
	amp2 = np.abs( np.fft.rfftn( img2 ) )

	return np.fft.irfftn( amp2 * ( np.cos( phi1 ) + 1j*np.sin( phi1 ) ) )

def SoftMask( imsize = [100, 100], radius = 0.5, width = 6.0, rounding=False, xyz=[0,0,0], rfft=False ):
# Generates a circular or spherical mask with a soft cosine edge
# If rfft==True, the output shape will not be imsize, be the shape of an rfft of input shape imsize

	if np.isscalar(imsize):

		imsize = [imsize, imsize]

	if len(imsize) > 3:

		raise ValueError ( "Object should have 2 or 3 dimensions: len(imsize) = %d " % len(imsize))

	rmesh = RadialIndices( imsize, rounding=rounding, xyz=xyz, rfft=rfft )[0]

	if width < 0.0:

		width = 0.0

	if width > 0.0 and width < 1.0:

		width = 0.5 * width * np.min( imsize )

	if radius > 0.0 and radius < 1.0:

		radius = radius * np.min( imsize )

		radius *= 0.5

	if ( radius < 0.0 ) or ( np.min( imsize ) < radius*2 ):

		radius = 0.5 * ( np.min( imsize ) - float(width)/2 )

	rii = radius + width/2
	rih = radius - width/2

	mask = np.zeros( rmesh.shape )

	fill_idx = rmesh <= rih
	mask[fill_idx] = 1.0

	rih_idx = rmesh > rih
	rii_idx = rmesh <= rii
	edge_idx = rih_idx * rii_idx

	mask[edge_idx] = ( 1.0 + np.cos( np.pi * ( rmesh[edge_idx] - rih ) / (width) ) ) / 2.0
	# print mask[edge_idx]

	return mask

def AutoMask( img, apix=1.0, lp=-1, gaussian=False, cosine=True, cosine_edge_width=3.0, absolute_threshold=None, fraction_threshold=None, sigma_threshold=1.0, expand_width = 3.0, expand_soft_width = 3.0, floodfill_rad=-1, floodfill_xyz=[0,0,0], floodfill_fraction=-1, verbose=False ):
# Creates a "smart" soft mask based on an input volume.
# Similar to EMAN2 mask.auto3d processor and relion_mask_create.

	# if type( lp ) == str:

	# 	if lp.lower() == 'auto':

	# 		# lp = 10 * apix # Auto low-pass filtering value (ad-hoc)
	# 		lp = 14.0 # Works well in most cases

	# First we low-pass filter the volume with a Gaussian or Cosine-edge filter:
	if gaussian:

		imglp = FilterGauss( img, apix=apix, lp=lp )
		filter_type = "Gaussian."

	else:

		imglp = FilterCosine( img, apix=apix, lp=lp, width=cosine_edge_width )
		filter_type = "cosine-edge across %.1f Fourier voxels." % cosine_edge_width


	# Then we define a threshold for binarization of the low-pass filtered map:
	if absolute_threshold != None:

		thr = absolute_threshold # Simply take a user-specified value as threshold
		method = "absolute value"

	elif fraction_threshold != None:

		thr = np.sort( np.ravel( imglp ) )[np.round( ( 1.0 - fraction_threshold ) * np.prod( imglp.shape ) ).astype( 'int' )] # Binarize the voxels with the top fraction_threshold densities
		method = "highest %.1f percent of densities" % ( fraction_threshold * 100 )

	elif sigma_threshold != None:

		thr = imglp.mean() + sigma_threshold * imglp.std() # Or define as threshold a multiple of standard deviations above the mean density value

		method = "%.3f standard deviations above the mean" % sigma_threshold

	else:

		thr = 0.0

	if verbose:

		print( "\nAUTO-MASKING INFO:" )
		print( "Input volume will be low-pass filtered at %.2f A by a %s" % ( lp, filter_type ) )
		print( "Stats of input volume before low-pass:\nMin=%.6f, Max=%.6f, Median=%.6f, Mean=%.6f, Std=%.6f" % ( img.min(), img.max(), np.median( img ), img.mean(), img.std() ) )
		print( "Stats of input volume after low-pass (for binarization):\nMin=%.6f, Max=%.6f, Median=%.6f, Mean=%.6f, Std=%.6f" % ( imglp.min(), imglp.max(), np.median( imglp ), imglp.mean(), imglp.std() ) )
		print( "Thresholding method: %s" % method)
		print( "Threshold for initial binarization: %.6f" % thr )
		print( "Binary mask will be expanded by %.1f voxels plus a soft cosine-edge of %.1f voxels." % (expand_width, expand_soft_width) )

	if floodfill_rad < 0 and floodfill_fraction < 0:

		if verbose:

			print( "Binarizing the low-pass filtered volume..." )

		imglpbin = imglp > thr # Binarize the low-pass filtered map with one of the thresholds above

	else:

		if floodfill_fraction < 0:

			if verbose:

				print( "Initializing flood-filling method with a sphere of radius %.1f voxels placed at [%d, %d, %d]..." % ( floodfill_rad, floodfill_xyz[0], floodfill_xyz[1], floodfill_xyz[2] ) )

			inimask = SoftMask( imglp.shape, radius=floodfill_rad, width=0, xyz=floodfill_xyz ) # This will be the initial mask

		else:

			if verbose:

				print( "Initializing flood-filling method binarizing the highest %.1f percent of densities..." % ( floodfill_fraction * 100 ) )

			floodfill_fraction_thr = np.sort( np.ravel( imglp ) )[np.round( ( 1.0 - floodfill_fraction ) * np.prod( imglp.shape ) ).astype( 'int' )] # Binarize the voxels with the top floodfill_fraction densities

			inimask = imglp > floodfill_fraction_thr # Binarize the low-pass filtered map with one of the thresholds above

		imglpbin = FloodFilling( imglp, inimask, thr=thr ) # Binarize the low-pass filtered map using flood-filling approach, works better on non low-pass filtered volumes.

	if expand_width > 0:

		expand_kernel = SoftMask( imglp.shape, radius=expand_width, width=0 ) # Creates a kernel for expanding the binary mask

		mask_expanded = np.fft.fftshift( np.fft.irfftn( np.fft.rfftn( imglpbin ) * np.fft.rfftn( expand_kernel ) ).real ) > 1e-6 # To prevent residual non-zeros from FFTs

	else:

		mask_expanded = imglpbin

	mask_expanded_prev = mask_expanded
	mask_expanded_soft = mask_expanded

	# Expanding with a soft-edge is the same as above but in a loop, 1-voxel-shell at a time, multiplying by a cosine:
	expand_kernel = SoftMask( mask_expanded_prev.shape, radius=1, width=0 )
	if expand_soft_width > 0:

		for i in np.arange( 1, np.round( expand_soft_width ) + 1 ):
		# for i in np.arange( np.round( expand_soft_width ) ):

			mask_expanded_new = ( np.fft.fftshift( np.fft.irfftn( np.fft.rfftn( mask_expanded_prev ) * np.fft.rfftn( expand_kernel ) ).real ) > 1e-6 ).astype( 'float' )  # To prevent residual non-zeros from FFTs

			mask_expanded_soft = mask_expanded_soft + ( mask_expanded_new - mask_expanded_prev ) * ( 1.0 + np.cos( np.pi * i / (expand_soft_width + 1) ) ) / 2.0
			# print ( 1.0 + np.cos( np.pi * ( i ) / (expand_soft_width+1) ) ) / 2.0

			mask_expanded_prev = mask_expanded_new

	if verbose:

		print( "Auto-masking done!\n" )

	return mask_expanded_soft

def FloodFilling( img, inimask, thr=0.0 ):
# "Smart" growing of binary volume based on flood-filling algorithm.
# Similar to mask.auto3d processor in EMAN2
	
	# mask = SoftMask( img.shape, radius=rad, width=0, xyz=xyz ) # This will be the initial mask
	mask = inimask
	expand_kernel = SoftMask( img.shape, radius=1, width=0 )
	# print np.sum(mask)

	mask_expanded_prev = mask
	r = 1
	while True:

		# spherenew = SoftMask( img.shape, radius=rad+r, width=0, xyz=xyz )
		mask_expanded_new = np.fft.fftshift( np.fft.irfftn( np.fft.rfftn( mask_expanded_prev ) * np.fft.rfftn( expand_kernel ) ).real ) > 1e-6  # To prevent residual non-zeros from FFTSs
		shell = mask_expanded_new - mask_expanded_prev
		imgshell = img * shell
		shellbin = imgshell > thr
		if np.any( shellbin ):

			mask = mask + shellbin
			mask_expanded_prev = mask

			# print("Expanded mask by %d voxels..." % r)
			# print("Total voxels included: %d" % mask.sum())

		else:

			break

		r += 1

	return mask

def CistemAutoMask( img, apix=1.0 ):

	p = img.mean()

	imgthr = img
	imgthr[img < p] = p
	imgthrlp = FilterCosine( imgthr,lp=50.0, apix=apix )

	pfilt = imgthrlp.mean()

	max500 = np.sort(imgthrlp.ravel())[::-1][:500]
	pmax500 = max500.mean()

	t = pfilt + 0.03 * ( pmax500 - pfilt ) # Equation 22 of Grant, Rohou & Grigorieff, eLIFE 2018

	mask = imgthrlp >= t

	return mask.astype( 'float32' )



def FilterGauss( img, apix=1.0, lp=-1, hp=-1, return_filter=False ):
# Gaussian band-pass filtering of images.

	rmesh = RadialIndices( img.shape, rounding=False, normalize=True, rfft=True )[0] / apix
	rmesh2 = rmesh*rmesh

	if lp <= 0.0:

		lowpass = 1.0

	else:

		# lowpass = np.exp( - lp ** 2 * rmesh2 / 2 )
		lowpass = np.exp( - lp ** 2 * rmesh2 )

	if hp <= 0.0:

		highpass = 1.0

	else:

		# highpass = 1.0 - np.exp( - hp ** 2 * rmesh2 / 2 )
		highpass = 1.0 - np.exp( - hp ** 2 * rmesh2 )

	bandpass = lowpass * highpass

	ft = np.fft.rfftn( img )

	filtered = np.fft.irfftn( ft * bandpass, s=img.shape )

	if return_filter:

		return filtered, bandpass

	else:

		return filtered

def FilterWhiten( img, return_filter=False, ps=False ):
# Whitens the spectrum of an image or map, i.e. the radial amplitude spectrum becomes 1.0 at all frequencies.
# 'ps' is a switch to whiten the Power Spectrum (radial squared amplitudes profile) instead of the amplitude spectrum.
# Which one to use depends on the application, but the difference in practice is barely noticeable.

	ft = np.fft.fftshift( np.fft.fftn( img ) )

	if ps:

		radprof = np.sqrt( RotationalAverage( ( ft * np.conj( ft ) ).real ) )

	else:

		radprof = RotationalAverage( np.abs( ft ) )

	filtered = np.fft.ifftn( np.fft.ifftshift( ft / radprof ) ).real

	if return_filter:

		return filtered, radprof

	else:

		return filtered

def FilterBfactor( img, apix=1.0, B=0.0, return_filter=False ):
# Applies a B-factor to images. B can be positive or negative.

	rmesh = RadialIndices( img.shape, rounding=False, normalize=True, rfft=True )[0] / apix
	rmesh2 = rmesh*rmesh

	bfac = np.exp( - (B * rmesh2  ) /  4  )

	ft = np.fft.rfftn( img )

	filtered = np.fft.irfftn( ft * bfac, s=img.shape )

	if return_filter:

		return filtered, bfac

	else:

		return filtered

def FilterDoseWeight( stack, apix=1.0, frame_dose=1.0, pre_dose = 0.0, total_dose=-1, kv=300.0 ):
# Applies Dose-Weighting filter on a stack of movie frames (Grant & Grigorieff, eLife 2015)
# stack is sequence of aligned movie frames (e.g. as generated by MotionCor2)
# frame_dose is the dose-per-frame (electrons per A^2)
# pre_dose is the total dose to which the movie has been pre-exposed before the first frame (e.g. if the first frame was discarded by MotionCor2)
# total_dose is the desired total dose for the dose-weighted average. If <= zero, will be the total dose of the movie.

	
	n_frames = stack.shape[0]

	if total_dose <= 0:

		total_dose = frame_dose * n_frames

	rmesh = RadialIndices( stack[0].shape, rounding=False, normalize=True, rfft=True )[0] / apix
	# rmesh2 = rmesh*rmesh


	a = 0.245
	b = -1.665
	c = 2.81

	critical_dose = a * ( rmesh ** b ) + c  # Determined at 300 kV
	# kv_factor = 1.0 - ( 300.0 - kv ) * ( 1.0 - 0.8 ) / ( 300.0 - 200.0 ) # This linear approximation is valid in the range 200 - 300 kV, probably not outside it
	kv_factor = 1.0 - ( 300.0 - kv ) * 0.002
	critical_dose *= kv_factor
	optimal_dose = 2.51284 * critical_dose # See electron_dose.f90 in Unblur source code for derivation details, and the paper as well where it says it is ~2.5x the critical_dose

	sum_q2 = np.zeros( rmesh.shape )
	dw_filtered = np.zeros( rmesh.shape ).astype( 'complex128' )
	for i in np.arange( 1, n_frames + 1 ):

		current_dose = ( i * frame_dose ) + pre_dose

		dose_diff = current_dose - total_dose
		if dose_diff > 0 and dose_diff < frame_dose:

			current_dose = total_dose

			stack[i-1] *= ( frame_dose - dose_diff ) / frame_dose # We may have to downweight the last frame to be added to achieve exactly the desired total dose

		if current_dose <= total_dose:
			
			q = np.exp( -0.5 * current_dose / critical_dose )
			q[ optimal_dose < current_dose ] = 0.0 # We cut out all frequencies that have exceeded the optimal dose in the current frame, because it would add just noise

			dw_filtered += q * np.fft.rfftn( stack[i-1] )
			sum_q2 += q * q

	dw_filtered /= np.sqrt( sum_q2 ) # Eq. 9

	dw_avg = np.fft.irfftn( dw_filtered, s=stack[0].shape )

	# if return_filter:

	# 	return dw_avg, bfac

	# else:

	return dw_avg

def FilterCosine( img, apix=1.0, lp=-1, hp=-1, width=6.0, return_filter=False ):
# Band-pass filtering of images with a cosine edge. Good to approximate a top-hat filter while still reducing edge artifacts.

	if width < 0.0:

		width = 0.0

	if width > 0.0 and width <= 1.0:

		width = 0.5 * width * np.min( img.shape )

	if lp <= 0.0:

		lowpass = 1.0

	else:

		lowpass = SoftMask( img.shape, radius=np.min( img.shape ) * apix/lp, width=width, rfft=True )

	if hp <= 0.0:

		highpass = 1.0

	else:

		highpass = 1.0 - SoftMask( img.shape, radius=np.min( img.shape ) * apix/hp, width=width, rfft=True )

	bandpass = lowpass * highpass

	# ft = np.fft.fftshift( np.fft.fftn( img ) )

	# filtered = np.fft.ifftn( np.fft.ifftshift( ft * bandpass ) )

	ft = np.fft.rfftn( img )

	# print ft.shape, bandpass.shape

	filtered = np.fft.irfftn( ft * bandpass )

	if return_filter:

		return filtered.real, bandpass

	else:

		return filtered.real

def FilterTophat( img, apix=1.0, lp=-1, hp=-1, return_filter=False ):
# Just a wrapper to the cosine filter with a hard edge:

	return FilterCosine( img, apix=apix, lp=lp, hp=hp, width=0.0, return_filter=False )

def HighResolutionNoiseSubstitution( img, apix=1.0, lp=-1, parallel=False ):
# Randomizes the phases of a map beyond resolution 'lp'
# If calling many times in parallel, make sure to set the 'parallel' flag to True

	# Get resolution shells:
	rmesh = RadialIndices( img.shape, rounding=False, normalize=True, rfft=True )[0] / apix

	lp = 1.0/lp

	ft = np.fft.rfftn( img )

	# Decompose Fourier transform into amplitudes and phases:
	amps = np.absolute( ft )
	phases = np.angle( ft )

	idx = rmesh > lp # Select only terms beyond desired resolution (not inclusive)

	if lp > 0.0:

		if parallel:
			
			# Just to make sure that parallel jobs launched nearly at the same time won't get the same seed
			np.random.seed()

		# numpy.random.seed( seed=123 ) # We have to enforce the random seed otherwise different runs would not be comparable
		phasesrnd = np.random.random( phases.shape ) * 2.0 * np.pi # Generate random phases in radians

		phases[idx] = phasesrnd[idx]

	ftnew = amps * ( np.cos( phases ) + 1j*np.sin( phases ) )

	return np.fft.irfftn( ftnew, s=img.shape )

def Resample( img, newsize=None, apix=1.0, newapix=None ):
# Resizes a real image or volume by cropping/padding its Fourier Transform, i.e. resampling.

	size = np.array( img.shape )

	if np.any( newsize == None ) and newapix == None:

		newsize = img.shape
		newapix = apix

	elif newapix != None:
		# This will be the new image size:
		newsize = np.round( size * apix / newapix ).astype( 'int' )

	# First calculate the forward FT:
	ft = np.fft.fftn( img )
	# Now FFT-shift to have the zero-frequency in the center:
	ft = np.fft.fftshift( ft )

	# Crop or pad the FT to obtain the new sampling:
	ft = Resize( ft, newsize )

	# Restore the ordering of the FT as expected by ifftn:
	ft = np.fft.ifftshift( ft )

	# We invert the cropped-or-padded FT to get the desired result, only the real part to be on the safe side:
	return np.fft.ifftn( ft  ).real

def NormalizeImg( img, mean=0.0, std=1.0, radius=-1 ):
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

# def FCC( volume1, volume2, phiArray = [0.0], invertCone = False, xy_only = False, z_only = False ):
def FCC( volume1, volume2, phiArray = [0.0], invertCone = False ):
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

		m = np.mod(volume1.shape, 2) # Check if dimensions are odd or even

		if volume1.ndim == 3:

			[M,N,P] = volume1.shape
			[zmesh, ymesh, xmesh] = np.mgrid[ -M/2:M/2, -N/2:N/2, -P/2:P/2  ]
			# # The below is for RFFT implementation which is faster but gives numerically different results that potentially affect resolution estimation, DO NOT USE.
			# # The above is consistent with other programs such as FREALIGN v9.11 and relion_postprocess.
			# [zmesh, ymesh, xmesh] = np.mgrid[-M//2+m[0]:(M-1)//2+1, -N//2+m[1]:(N-1)//2+1, 0:P//2+1]
			# zmesh = np.fft.ifftshift( zmesh )
			# ymesh = np.fft.ifftshift( ymesh )

			rhomax = np.int( np.ceil( np.sqrt( M*M/4.0 + N*N/4.0 + P*P/4.0) ) + 1 )
			# if xy_only:
			# 	zmesh *= 0
			# 	rhomax = np.int( np.ceil( np.sqrt( N*N/4.0 + P*P/4.0) ) + 1 )
			# if z_only:
			# 	xmesh *= 0
			# 	ymesh *= 0
			# 	rhomax = rhomax = np.int( np.ceil( np.sqrt( M*M/4.0 ) ) + 1 )
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

	# # RFFT implementation faster but gives numerically different results that potentially affect resolution estimation, DO NOT USE.
	# # The above is consistent with other programs such as FREALIGN v9.11 and relion_postprocess.
	# fft1 = np.ravel( np.fft.rfftn( volume1 ) )
	# conj_fft2 = np.ravel( np.fft.rfftn( volume2 ) ).conj()

	FCC_normed = np.zeros( [rhomax, len(phiArray)] )
	for J, phiAngle in enumerate( phiArray ):

		if phiAngle == 0.0:
			fft1_conic = fft1
			conj_fft2_conic = conj_fft2
			rhoround_conic = rhoround
		else:
			conic = np.ravel( (phimesh <= phiAngle ) + ( (np.abs(phimesh - np.pi)) <= phiAngle )  )
			if invertCone:
				conic = np.invert( conic )
			rhoround_conic = rhoround[conic]
			fft1_conic = fft1[conic]
			conj_fft2_conic = conj_fft2[conic]

		FCC = np.bincount( rhoround_conic, np.real(fft1_conic * conj_fft2_conic) )
		Norm1 = np.bincount( rhoround_conic, np.abs(fft1_conic)*np.abs(fft1_conic) )
		Norm2 = np.bincount( rhoround_conic, np.abs(conj_fft2_conic)*np.abs(conj_fft2_conic) )

		goodIndices = np.argwhere( (Norm1 * Norm2) > 0.0 )[:-1]
		FCC_normed[goodIndices,J] = FCC[goodIndices] / np.sqrt( Norm1[goodIndices] * Norm2[goodIndices] )

	return FCC_normed

def FSC( volume1, volume2, phiArray = [0.0] ):
# FSC is just a wrapper to FCC

	return FCC( volume1, volume2, phiArray = phiArray )

def FRC( image1, image2, phiArray = [0.0] ):
# FSC is just a wrapper to FRC

	return FCC( image1, image2, phiArray = phiArray )

def Resize( img, newsize=None, padval=None, xyz=[0,0,0] ):
# Resizes a real image or volume by cropping/padding. I.e. sampling is not changed.
# xyz is the origin or cropping the image (does not apply to padding)

	xyz = -np.flipud( xyz ) # The minus sign is to ensure the same conventions are followed as for RadialIndices() function.

	if np.any( newsize == None ):

		return img

	else:

		imgshape = np.array( img.shape )
		newshape = np.round( np.array( newsize ) ).astype( 'int' )

		if np.all( imgshape == newshape ):

			return img

		if padval == None:

			padval = 0

		if len( imgshape ) == 2:

			if newshape[0] <= imgshape[0]:

				newimg = img[imgshape[0]/2-newshape[0]/2-xyz[0]:imgshape[0]/2+newshape[0]/2+newshape[0]%2-xyz[0], :]

			else:

				newimg = np.pad( img, ( ( newshape[0]/2-imgshape[0]/2, newshape[0]/2-imgshape[0]/2+newshape[0]%2 ), ( 0, 0 ) ), 'constant', constant_values = ( padval, ) )

			if newshape[1] <= imgshape[1]:

				newimg = newimg[:, imgshape[1]/2-newshape[1]/2-xyz[1]:imgshape[1]/2+newshape[1]/2+newshape[1]%2-xyz[1]]

			else:

				newimg = np.pad( newimg, ( ( 0, 0 ), ( newshape[1]/2-imgshape[1]/2, newshape[1]/2-imgshape[1]/2+newshape[1]%2 ) ), 'constant', constant_values = ( padval, ) )

			return newimg[:newshape[0],:newshape[1]]

		elif len( imgshape ) == 3:

			if newshape[0] <= imgshape[0]:

				newimg = img[imgshape[0]/2-newshape[0]/2-xyz[0]:imgshape[0]/2+newshape[0]/2+newshape[0]%2-xyz[0], :, :]

			else:

				newimg = np.pad( img, ( ( newshape[0]/2-imgshape[0]/2, newshape[0]/2-imgshape[0]/2+newshape[0]%2 ), ( 0, 0 ), ( 0, 0 ) ), 'constant', constant_values = ( padval, ) )

			if newshape[1] <= imgshape[1]:

				newimg = newimg[:, imgshape[1]/2-newshape[1]/2-xyz[1]:imgshape[1]/2+newshape[1]/2+newshape[1]%2-xyz[1], :]

			else:

				newimg = np.pad( newimg, ( ( 0, 0 ), ( newshape[1]/2-imgshape[1]/2, newshape[1]/2-imgshape[1]/2+newshape[1]%2 ), ( 0, 0 ) ), 'constant', constant_values = ( padval, ) )

			if newshape[2] <= imgshape[2]:

				newimg = newimg[:, :, imgshape[2]/2-newshape[2]/2-xyz[2]:imgshape[2]/2+newshape[2]/2+newshape[2]%2-xyz[2]]

			else:

				newimg = np.pad( newimg, ( ( 0, 0 ), ( 0, 0 ), ( newshape[2]/2-imgshape[2]/2, newshape[2]/2-imgshape[2]/2+newshape[2]%2 ) ), 'constant', constant_values = ( padval, ) )

			return newimg[:newshape[0],:newshape[1],:newshape[2]]

		else:

			raise ValueError( "Object should have 2 or 3 dimensions: len(imgshape) = %d " % len(imgshape))

def SigmaCurve( imsize = [100, 100], sigma = 3.0, nsym = 1, D = 2.0, L = 3.0, count=False ):
# Generates the Sigma criterion curve (e.g. 3-sigma)
# Harauz & van Heel, Optik 1986
# imsize = dimensions of image or volume
# sigma = desired number of standard deviations above noise (Default: 3)
# nsym = number of asymmetric units present in the volume
# D = linear size of the object (approx. diameter of particle or length along longest dimension)
# L = linear size of the volume (box side)

	# Eq. 19 in (van Heel & Schatz, JSB 2005):
	return 2 * L * sigma * np.sqrt( nsym ) / ( 3 * D * np.sqrt( VoxelsPerShell( imsize, count=count ) / 2.0 ) )

def HalfBitCurve( imsize = [100, 100], nsym = 1, D = 2.0, L = 3.0, count=False ):
# Generates the 1/2-bit criterion curve
# van Heel % Schatz, JSB 2005
# imsize = dimensions of image or volume
# nsym = number of asymmetric units present in the volume
# D = linear size of the object (approx. diameter of particle or length along longest dimension)
# L = linear size of the volume (box side)

	# Eq. 18 in (van Heel & Schatz, JSB 2005):
	n_eff = ( VoxelsPerShell( imsize, count=count ) * ( 3 * D / ( 2 * L) ) ** 2 ) / ( 2 * nsym )

	# Eq. 17 in (van Heel & Schatz, JSB 2005):
	return ( 0.2071 + 1.9102 * 1 / np.sqrt( n_eff ) ) / ( 1.2071 + 0.9102 * 1 / np.sqrt( n_eff ) )

def VoxelsPerShell( imsize = [100, 100], count=False ):
# Calculates the number of pixels/voxels per radial ring/shell of an mage/volume (usually representing Fourier space)
# 'count', if False, calculates the number of voxels acording to the formula of circunference or spherical surface area (prettier for plotting). If True, it will actually count the number of voxels doing rounding, that is, it will be consistent with the FRC/FSC. Both methods agree asymptotically, they only differ for small radii (i.e. closer to the origin, low resolution regime in reciprocal space)

	if not count:

		NSAM = np.round( np.sqrt( np.sum( np.power( imsize, 2 ) ) ) / 2.0 / np.sqrt( len( imsize ) ) ).astype('int') + 1 # For cubic volumes this is just half the box size + 1.

		# print np.arange( 0, NSAM)
		# print np.unique(RadialIndices( imsize )[0])

		if len( imsize ) == 3:

			nvoxels = 4 * np.pi * np.arange( 0, NSAM) ** 2

		elif len( imsize ) == 2:

			nvoxels = 2 * np.pi * np.arange( 0, NSAM)

		else:

			raise ValueError( "Object should have 2 or 3 dimensions: len(imsize) = %d " % len( imsize )  )

		nvoxels[0] = 1

	else:

		rmesh = RadialIndices( imsize )[0]

		nvoxels = np.bincount( rmesh.ravel() )

	return nvoxels




def CrossCorrelation( img1, img2 ):
# Calculates the normalized cross-correlation between two vectors (e.g. images or volumes)
# Inputs can be real or complex

	f1 = np.ravel( img1 - img1.mean() )
	f2 = np.conj( np.ravel( img2 - img2.mean() ) )

	return np.sum( np.real( f1 * f2 ) ) / np.sqrt( ( np.sum( np.abs( f1 ) ** 2) * np.sum( np.abs( f2 )**2 ) ) )

def BandPassCrossCorrelation( img1, img2, apix=1.0, lp=-1, hp=-1, weights=None, rounding=True, eps=1e-8 ):
# Calculates the normalized cross-correlation between two images or volumes considering only the frequency range specified.
# Optionally, weights can be specified as a radial filter.
# Rounding is True by default, to ensure consistency with FSC/FRC; but can be disabled if more accuracy (discrimination power) is required.

	if weights != None:

		img1 = RadialFilter( img1, weights, return_filter = False )

	ft1 = np.fft.fftshift( np.fft.fftn( img1 ) )
	ft2 = np.fft.fftshift( np.fft.fftn( img2 ) )

	if lp <= 0.0:

		lowpass = np.ones( ft1.shape )

	else:

		lowpass = SoftMask( img1.shape, radius=np.min( img1.shape ) * apix/lp, width=0, rfft=False, rounding=rounding )

	if hp <= 0.0:

		highpass = 1.0

	else:

		highpass = 1.0 - SoftMask( img1.shape, radius=np.min( img1.shape ) * apix / (hp + eps), width=0, rfft=False, rounding=rounding ) # eps to ensure this frequency is included

	bandpass = ( lowpass * highpass ).astype( 'bool')

	return CrossCorrelation( ft1[bandpass], ft2[bandpass] )

def ResolutionAtThreshold(freq, fsc, thr, interp=True, nyquist_is_fine=False, ):
# Do a simple linear interpolation (optional) to get resolution value at the specified FSC threshold

	if np.isscalar( thr ):

		thr *= np.ones( fsc.shape )

	# i = 0
	for i,f in enumerate( fsc ):

		# if f < thr and i > 0:
		if f < thr[i]:

			break

		# i += 1

	if i < len(fsc)-1 and i > 1:

		if interp:

			y1 = fsc[i]
			y0 = fsc[i-1]
			x1 = freq[i]
			x0 = freq[i-1]

			delta = (y1-y0)/(x1-x0)

			res_freq = x0 + (thr[i-1] - y0) / delta

		else:
		
			# Just return the highest resolution bin at which FSC is still higher than threshold:
			res_freq = freq[i-1]

	elif i == 0:

		res_freq = freq[i]

	else:

		res_freq = freq[-1]

		if not nyquist_is_fine:

			print( '\nFSC NEVER DROPS BELOW %.3f THRESHOLD. THERE IS SOMETHING WRONG!!!\n' % thr[0] )
			print( 'Possible reasons include:' )
			print( '-You provided the same file as map1 and map2 by accident;' )
			print( '-Your mask has problems such as being too tight or cutting through actual protein density.Using --randomize_below_fsc can correct for such distortions on the FSC, but ultimately you should use a generous mask with a soft edge for more reliable results;' )
			print( '-You provided the wrong molecular weight for your particle (if using --mw);' )
			print( '-You have downsampled (or binned) your data, then the problem should go away once you calculate the FSC between the full-resolution reconstructions;' )
			print( '-Your data is heavily undersampled, e.g. by operating the TEM at a too low magnification (large pixel size);' )
			print( '-Your data suffers from severe reference bias, or other kind of systematic artefact in your algorithms. This is very serious and should be investigated carefully.\n' )


	return 1/res_freq

def Project( img, pose = [0,0,0,0,0], interpolation='trilinear', pad=2, do_sinc=True, res_max=-1, apix=-1, is_fft=False, DF1 = 1000.0, DF2 = None, AST = 0.0, WGH = 0.10, invert_contrast = False, Cs = 2.7, kV = 300.0, phase_flip = False, ctf_multiply = False  ):
# Projects a 3D volume to a 2D image, with optional CTF correction
# Consistent with relion_project
# is_fft to be used if the input is already an FFT (with fftshift applied!)
# pose = [ROT (PHI), TILT, PSI, SHX, SHY]

	pose = np.array( pose, dtype='float32' )
	imsize = np.array( img.shape )
	rot = pose[:3]
	shift = [-pose[4],-pose[3]] # To be consistent with relion_project

	if res_max > 0.0 and apix <= 0:

		raise ValueError( "Pixel size must be specified for option res_max to work!" )

	if not is_fft:

		if do_sinc:

			rmesh = RadialIndices( img.shape, rounding=False, normalize=True, rfft=False )[0] / pad

			sinc = np.sinc( rmesh )

			if interpolation == 'nearest':

				imgc = img / sinc

			elif interpolation == 'trilinear':

				imgc = img / ( sinc * sinc )

		imgpad = np.fft.fftshift( Resize( imgc, newsize=imsize * pad ) ) # Pad the real-space image, and FFT-shift the result for proper centering of the phases in subsequent operations
		del img,imgc

		F = np.fft.fftshift( np.fft.fftn( imgpad ) )# Do the actual FFT of the input
		imsizepad = np.array( imgpad.shape )
		del imgpad

	else:

		F = img
		imsizepad = imsize

	Frot = Rotate( F, rot, interpolation=interpolation, pad=1 ) # Do the actual rotation of the FFT
	del F
    
    # Calculate the projection:
	Fslice = Frot[imsizepad[0]//2,:,:] # Extract the central slice (i.e. projection for weak-phase object approximation)
	del Frot
    
    # Below, zmesh is ignored because the projection is invariant to shifts along Z- (and translations are applied AFTER rotation):
	m = np.mod(imsizepad, 2) # Check if dimensions are odd or even
	[xmesh, ymesh] = np.mgrid[-imsizepad[0]//2+m[0]:(imsizepad[0]-1)//2+1, -imsizepad[1]//2+m[1]:(imsizepad[1]-1)//2+1]
# 	[xmesh, ymesh,zmesh] = np.mgrid[-imsizepad[0]//2+m[0]:(imsizepad[0]-1)//2+1, -imsizepad[1]//2+m[1]:(imsizepad[1]-1)//2+1, -imsizepad[2]//2+m[2]:(imsizepad[2]-1)//2+1]
	xmesh = np.fft.ifftshift( xmesh )
	ymesh = np.fft.ifftshift( ymesh )
# 	zmesh = np.fft.ifftshift( zmesh )

	Fslice *= np.exp( -2.0 * np.pi * 1j * ( shift[0] * xmesh / imsizepad[0] + shift[1] * ymesh / imsizepad[1] ) )

	# Direct CTF correction would invert the image contrast. By default we don't do that, hence the negative sign:
	CTFim = np.fft.fftshift( focus_ctf.CTF( Fslice.shape, DF1, DF2, AST, WGH, Cs, kV, apix, 0.0, rfft=False ) )

	if invert_contrast:

		CTFim *= -1.0

	if phase_flip: # Phase-flipping

		Fslice *= np.sign( CTFim )

	if ctf_multiply: # CTF multiplication

		Fslice *= CTFim

	if res_max > 0.0:

		lowpass = SoftMask( Fslice.shape, radius=np.min( Fslice.shape ) * apix / res_max, width=0, rfft=False )
		Fslice *= lowpass
		del lowpass

	if not is_fft:

		I = np.fft.ifftn( np.fft.ifftshift( Fslice ) ).real # FFT-back the result to real space
		del Fslice

		return Resize( np.fft.ifftshift( I ), newsize=imsize[:2] ) # Undo the initial FFT-shift in real space and crop to the original size of the input

	else:

		return Fslice

	
def BackProject( img, pose = [0,0,0,0,0], interpolation='trilinear', pad=2, do_sinc=True, res_max=-1, apix=-1, return_weights=True, is_fft=False, DF1 = 1000.0, DF2 = None, AST = 0.0, WGH = 0.10, invert_contrast = False, Cs = 2.7, kV = 300.0, phase_flip = False, ctf_multiply = False, wiener_filter = False, C = 1.0 ):
# Consistent with relion_project
# is_fft to be used if the input is already an FFT (with fftshift applied!)
# pose = [ROT (PHI), TILT, PSI, SHX, SHY]
# Note: for a 2D image projected from 3D with orientation [ROT, TILT, PSI, SHX, SHY], it has to be back-projected to 3D at orientation [-PSI, -TILT, -PHI, -SHX, -SHY]

	pose = np.array( pose, dtype='float32' )
	imsize = np.array( img.shape )
	rot = pose[:3]
	shift = [-pose[4],-pose[3]] # To be consistent with relion_project

	if res_max > 0.0 and apix <= 0:

		raise ValueError( "Pixel size must be specified for option res_max to work!" )

	if not is_fft:

		if do_sinc:

			rmesh = RadialIndices( img.shape, rounding=False, normalize=True, rfft=False )[0] / pad

			sinc = np.sinc( rmesh )

			if interpolation == 'nearest':

				imgc = img / sinc

			elif interpolation == 'trilinear':

				imgc = img / ( sinc * sinc )

		imgpad = np.fft.fftshift( Resize( imgc, newsize=imsize * pad ) ) # Pad the real-space image, and FFT-shift the result for proper centering of the phases in subsequent operations
		del img,imgc

		Fslice = np.fft.fftshift( np.fft.fftn( imgpad ) )# Do the actual FFT of the input
		imsizepad = np.array( imgpad.shape )
		del imgpad

	else:

		Fslice = img
		imsizepad = imsize

	if phase_flip or ctf_multiply or wiener_filter:

		# Direct CTF correction would invert the image contrast. By default we don't do that, hence the negative sign:
		CTFim = np.fft.fftshift( focus_ctf.CTF( Fslice.shape, DF1, DF2, AST, WGH, Cs, kV, apix, 0.0, rfft=False ) )

		if invert_contrast:

			CTFim *= -1.0

	if phase_flip: # Phase-flipping

		pf_filt = np.sign( CTFim )

		Fslice *= pf_filt
		if return_weights:
			weights = np.abs( pf_filt )

	elif ctf_multiply: # CTF multiplication

		Fslice *= CTFim
		if return_weights:
			weights = np.abs( CTFim )

	elif wiener_filter: # Wiener filtering

		if C <= 0.0:

			raise ValueError( "Error: Wiener filter constant cannot be less than or equal to zero! C = %f " % C )

		wienerfilt = CTFim / ( CTFim*CTFim + C )
		Fslice *= wienerfilt
		if return_weights:
			weights = np.abs( wienerfilt )

	elif return_weights:

		weights = np.ones( ( imsizepad[0], imsizepad[1] ), dtype='float32' )

	# Below, zmesh is ignored because the projection is invariant to shifts along Z- (and translations are applied AFTER rotation):
	m = np.mod(imsizepad, 2) # Check if dimensions are odd or even
	[xmesh, ymesh] = np.mgrid[-imsizepad[0]//2+m[0]:(imsizepad[0]-1)//2+1, -imsizepad[1]//2+m[1]:(imsizepad[1]-1)//2+1]
# 	[xmesh, ymesh,zmesh] = np.mgrid[-imsizepad[0]//2+m[0]:(imsizepad[0]-1)//2+1, -imsizepad[1]//2+m[1]:(imsizepad[1]-1)//2+1, -imsizepad[2]//2+m[2]:(imsizepad[2]-1)//2+1]
	xmesh = np.fft.ifftshift( xmesh )
	ymesh = np.fft.ifftshift( ymesh )
# 	zmesh = np.fft.ifftshift( zmesh )

	Fslice *= np.exp( -2.0 * np.pi * 1j * ( shift[0] * xmesh / imsizepad[0] + shift[1] * ymesh / imsizepad[1] ) )
	Fvol = np.zeros( ( imsizepad[0], imsizepad[0], imsizepad[1] ), dtype='complex64')
	Fvol[imsizepad[0]//2,:,:] = Fslice # Backproject: insert the central slice into the Fourier transform of the volume to be reconstructed
	del Fslice
	Frot = Rotate( Fvol, rot, interpolation=interpolation, pad=1 ) # Do the actual rotation of the FFT
	del Fvol
	if return_weights:

		Wvol = np.zeros( ( imsizepad[0], imsizepad[0], imsizepad[1] ), dtype='float32')
		Wvol[imsizepad[0]//2,:,:] = weights
		Wrot = Rotate( Wvol, rot, interpolation=interpolation, pad=1 ) # Do the actual rotation of the FFT
		del Wvol

	if res_max > 0.0:

		lowpass = SoftMask( Frot.shape, radius=np.min( Frot.shape ) * apix / res_max, width=0, rfft=False )
		Frot *= lowpass
		if return_weights:
			Wrot *= lowpass
		del lowpass

	if not is_fft:

		I = np.fft.ifftn( np.fft.ifftshift( Frot ) ).real # FFT-back the result to real space

		del Frot

		if return_weights:

			return Resize( np.fft.ifftshift( I ), newsize=[imsize[0],imsize[0],imsize[1]] ), weights # Undo the initial FFT-shift in real space and crop to the original size of the input

		else:

			return Resize( np.fft.ifftshift( I ), newsize=[imsize[0],imsize[0],imsize[1]] )

	else:

		if return_weights:

			return Frot, weights

		else:

			return Frot