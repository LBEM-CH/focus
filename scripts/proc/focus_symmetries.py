# Symmetry utilities in Python for Focus
# Author: Ricardo Righetto
# E-mail: ricardo.righetto@unibas.ch
# http://www.focus-em.org

import numpy as np
import focus_utilities as util
import copy

def ApplySymmetry( img, sym='c1', pad=1 ):
# Padding reduces interpolation artefacts at the expense of RAM and computation time


	imgshape_ori = img.shape
	if len( imgshape_ori ) == 2:

		img = img.reshape( (1, imgshape_ori[0], imgshape_ori[1] ) )

	imsize = np.array( img.shape )
	# print imsize

	if pad != 1:

		img = util.Resample( img, newsize=imsize * pad )

	[Z,Y,X] = np.mgrid[0:img.shape[0], 0:img.shape[1], 0:img.shape[2]]

	# Begin symmetry operations:

	# Symmetry operations that can be trivially obtained by coordinate manipulations are explicitly defined for speedup:
	if sym.lower() == 'c1':

		imgsym = img

	elif sym.lower() == 'c2':

		imgsym = copy.copy( img )

		imgsym += img[Z,-Y,-X]

		imgsym /= 2.0

	elif sym.lower() == 'c4':

		imgsym = copy.copy( img )

		imgsym += img[Z,-Y,-X]

		imgsym += img[Z,-X,Y]

		imgsym += img[Z,X,-Y]

		imgsym /= 4.0

	elif sym.lower() == 'd1':

		imgsym = copy.copy( img )

		imgsym += img[-Z,-Y,X]

		imgsym /= 2.0

	elif sym.lower() == 'd2':

		imgsym = copy.copy( img )

		imgsym += img[Z,-Y,-X]
		imgsym += imgsym[-Z,-Y,X]

		imgsym /= 4.0

	elif sym.lower() == 'd4':

		imgsym = copy.copy( img )

		imgsym += img[Z,-Y,-X]

		imgsym += img[Z,-X,Y]

		imgsym += img[Z,X,-Y]

		imgsym += imgsym[-Z,-Y,X]

		imgsym /= 8.0

	elif sym.lower() == 'p4212':

		imgsym = copy.copy( img )

		imgsym += img[Z,-Y,-X]

		Ynew_a = -Y + img.shape[1]//2
		Ynew_b = ( Ynew_a + 2 * Y ) % img.shape[1]
		Xnew_a = -X + img.shape[2]//2
		Xnew_b = ( Xnew_a + 2 * X ) % img.shape[2]

		imgsym += img[Z,Xnew_b,Ynew_a]

		imgsym += img[Z,Xnew_a,Ynew_b]

		imgsym += img[Z,Ynew_b,Xnew_a]

		imgsym += img[Z,Ynew_a,Xnew_b]

		imgsym += img[-Z,X,Y]

		imgsym += img[-Z,-X,-Y]

		imgsym /= 8.0

	elif sym[0].lower() == 'c':

		n = np.int( sym[1:] )
		# print n

		imgsym = copy.copy( img )
		ang = 360.0/n
		for k in np.arange( 1, n ):

			# print k * ang

			imgrot = util.Rotate( img, rot=[k * ang, 0, 0], interpolation='trilinear', pad=1 )
			# print np.allclose(imgrot, 0.0)
			imgsym += imgrot

		# print np.allclose(imgsym, img)

		imgsym /= n

	elif sym[0].lower() == 'd':

		n = np.int( sym[1:] )
		# print n

		imgsym = copy.copy( img )
		imgsym += img[-Z,Y,X]
		ang = 360.0/n
		for k in np.arange( 1, n + 1 ):

			# print k * ang

			imgrot = util.Rotate( img, rot=[k * ang, 0, 0], interpolation='trilinear', pad=1 )
			imgsym += imgrot
		
		imgsym += imgsym[-Z,-Y,X]

		imgsym /= 2 * n

	else:

		raise Exception( "Symmetry operator not supported! sym = %s" % sym )

	if pad != 1:

			imgsym = util.Resample( imgsym, newsize=imgshape_ori )

	if len( imgshape_ori ) == 2:

		imgsym = imgsym.reshape( ( imgshape_ori[0], imgshape_ori[1] ) )

	return imgsym