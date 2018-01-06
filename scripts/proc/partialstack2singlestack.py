#!/usr/bin/env python

#############################################################################
#                                                                           #
# Given a list of partial stack files, will append all particles to a 		#
# single stack            													#
#                                                                           #
# (C) 2dx.org, GNU Public License. 	                                        #
#                                                                           #
# Created..........: 10/01/2017                                             #
# Last Modification: 10/01/2017                                             #
# Author...........: Ricardo Righetto                                       #
# 																			#
#############################################################################

import sys
import numpy as np
from mrcz import ioMRC
import sparx as spx

def main():

	inputs = sys.argv[1:-1]
	output = sys.argv[-1]

	idx = 0
	m = 1
	M = len(inputs)
	for stack in inputs:

		N = spx.EMUtil.get_image_count(stack)

		for n in np.arange(N):

			img = spx.EMData()
			img.read_image(stack, n)

			# Write image to the particle stack:
			if idx == 0:

				# If this is the first image, we initiate as a normal .mrcs stack.
				img.write_image(output, idx)

			else:

				# Subsequent images are directly appended to the file:
				with open(output, 'ab') as mrcf:
					spx.EMNumPy.em2numpy(img).tofile(mrcf)

			idx += 1

			print 'Appended image %d/%d from stack %d/%d. Total images appended: %d.' % (n+1, N, m, M, idx)

		m += 1

	# Maybe it is better to update the NZ in header with the MRCheaderUpdate.py script?

	header = ioMRC.readMRCHeader( output )
	header['dimensions'][0] = idx
	# Now we write the header back:
	with open( output, 'rb+' ) as mrcf: 
		ioMRC.writeMRCHeader( mrcf, header, endchar = '<' )

	print 'Done!'


if __name__ == "__main__":
	main()