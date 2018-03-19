# Script to update the defocus values for every particle based on new tilt-geometry after pre-refinement.
### NEEDS FURTHER VALIDATION!!!

import numpy as np
import copy
import sys

def main():

	par_old = np.loadtxt( sys.argv[1], comments='C' )
	par_new = np.loadtxt( sys.argv[2], comments='C' )
	par_out = copy.deepcopy( par_new )
	coords = np.loadtxt( sys.argv[4], usecols=(0,1,3,4,5,6,7,8) )
	apix = float( sys.argv[5] )
	w = float( sys.argv[6] )

	tltaxis_old = 270.0 - par_old[:,1]
	tltaxis_new = 270.0 - par_new[:,1]
	tltang_old = par_old[:,2]
	tltang_new = par_new[:,2]

	U = np.unique( coords[:,1] )
	oddU = U[0::2] # .coords file convention starts at 1!
	evenU = U[1::2]
	oddcoords = coords[np.in1d( coords[:,1], oddU )]
	evencoords = coords[np.in1d( coords[:,1], evenU )]
	# print coords.shape
	# print oddcoords.shape
	# print evencoords.shape

	coords_interlaced = np.zeros( coords.shape, dtype=coords.dtype )
	# coords_interlaced[0::2,:] = oddcoords
	# coords_interlaced[1::2,:] = evencoords
	coords_interlaced[np.in1d( coords[:,1], oddU ),:] = oddcoords
	coords_interlaced[np.in1d( coords[:,1], evenU ),:] = evencoords

	xbox = coords_interlaced[:,2]
	ybox = coords_interlaced[:,3]
	box_size_x = coords_interlaced[:,4]
	box_size_y = coords_interlaced[:,5]
	x = xbox - w/2 + box_size_x/2
	y = ybox - w/2 + box_size_y/2

	mic_list = np.unique( par_old[:,7] )
	if not np.all(  mic_list == U ):

		print( 'Error with micrograph numbers!' )
		sys.exit( 1 )

	# for m in mic_list:

	# 	selpar = np.argwhere( par_old[:,7] == m )
	# 	selcoord = np.argwhere( coords[:,1] == m )

	# 	for i,j in enumerate( selpar ):

	def1_old,def2_old = CalculateDefocusTilted( x, y, apix, tltaxis_old, tltang_old, 0.0, 0.0 )
	def1_new,def2_new = CalculateDefocusTilted( x, y, apix, tltaxis_new, tltang_new, 0.0, 0.0 )
	# # If wanting to ignore the changes in tltaxis, which may be entangled between PSI and PHI:
	# def1_new,def2_new = CalculateDefocusTilted( x, y, apix, tltaxis_old, tltang_new, 0.0, 0.0 )

	par_out[:,8] += def1_new - def1_old
	par_out[:,9] += def2_new - def2_old

	np.savetxt( sys.argv[3], par_out, fmt='    %d    %.2f    %.2f    %.2f    %.2f    %.2f    %d    %d    %.2f    %.2f    %.2f    %.2f    %d    %.4f    %.2f    %.2f')

def CalculateDefocusTilted( x, y, apix, TLTAXIS, TLTANG, DEFOCUS1, DEFOCUS2 ):

	# Find defocus and astigmatism values in particle position (we know them at the image center):
	# rdist1 = np.sqrt(x**2 + y**2) # distance in pixels from image center to window center
	# rbeta = np.arctan2(y, x) * 180.0 / np.pi # angle in degrees between the x-axis and the line connecting image center to window center
	# rgamma = rbeta - TLTAXIS # angle between the line connecting image center to window center and tilt axis
	# rdist2 = rdist1 * np.sin(rgamma * np.pi / 180.0) # distance in pixels between window center and closest point on tilt axis
	# rdist3 = rdist2 * apix # distance in Angstroems
	# RLDEF1 = DEFOCUS1 + rdist3 * np.tan(TLTANG * np.pi / 180.0)
	# RLDEF2 = DEFOCUS2 + rdist3 * np.tan(TLTANG * np.pi / 180.0)

	# CTFTILT equation:
	# This formula is equivalent to the one above. We just change the sign of DF because our x,y are the negative of DX,DY.

	# DFL1  = DFMID1 +DF
	# DFL2  = DFMID2 +DF
	# DF    = (N1*DX+N2*DY)*PSIZE*TAN(TANGLE)
	# DX    = CX-NX
	# DY    = CY-NY
	# CX    = CENTER_X =         1904
	# CY    = CENTER_Y =         1904
	# PSIZE = PIXEL SIZE [A] =       1.3400
	# N1,N2 = TILT AXIS NORMAL:
	# N1 =  SIN(TLTAXIS) =    -0.995884
	# N2 = -COS(TLTAXIS) =    -0.090635

	N1 = np.sin(TLTAXIS * np.pi / 180.0)
	N2 = -np.cos(TLTAXIS * np.pi / 180.0)
	DF = -(N1 * x + N2 * y) * apix * np.tan(TLTANG * np.pi / 180.0)
	RLDEF1 = DEFOCUS1 + DF
	RLDEF2 = DEFOCUS2 + DF

	return RLDEF1,RLDEF2

if __name__ == '__main__':
	main()