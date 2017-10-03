#!/usr/bin/env python

#############################################################################
#                                                                           #
# Updates header of an MRC file to ensure correct number of Z slices		#
# or particles in stack                										#
#                                                                           #
# (C) 2dx.org, GNU Public License. 	                                        #
#                                                                           #
# Created..........: 09/10/2017                                             #
# Last Modification: 09/10/2017                                             #
# Author...........: Ricardo Righetto                                       #
# 																			#
# Most comprehensive description of MRC format:								#
# http://bio3d.colorado.edu/imod/doc/mrc_format.txt 						#
# 																			#
# This script is based on Robb McLeod's ioMRC.py. Source is hosted at:		#
# https://bitbucket.org/emmrcz/python-mrcz									#
#############################################################################

import sys
import os.path
import numpy as np
import ioMRC
from optparse import OptionParser

def main():

	progname = os.path.basename(sys.argv[0])
	usage = progname + """ [options] <.mrc(s) file>

	Given an existing .mrc(s) file, will evaluate its size and number of images/slices (NZ) and UPDATE IN-PLACE (CAUTION!) corresponding header information.

	Reference for MRC format description:
	http://bio3d.colorado.edu/imod/doc/mrc_format.txt

	This script is based on Robb McLeod's ioMRC.py available at:
	https://bitbucket.org/emmrcz/python-mrcz

	NOTE: Only vanilla MRC(S) files are supported (i.e. compressed MRC formats not supported)

	"""

	parser = OptionParser(usage)
	
	parser.add_option( "--stats", action="store_true", help="Will update the statistics of the file (minval, maxval, avgval, std). May take some time depending on file size.", default=False )
	parser.add_option( "--endian", metavar="le", type="string", default='le', help="MRC file endianness (default is little-endian)" )

	(options, args) = parser.parse_args()

	command = ' '.join(sys.argv)

	if options.endian == 'le':
		endchar = '<'
	else:
		endchar = '>'

	fpath = args[0] # The file to be updated
	hsize = 1024 # Standard MRC header is 1024 bytes long

	S = os.path.getsize( fpath ) # Get the current size of the file in bytes
	# print S-hsize
	header = ioMRC.readMRCHeader( fpath ) # Read in the current header of the file
	if 'meta' not in header.keys():
		header['meta'] = None
	
	sizeof = np.dtype(header['dtype']).itemsize # Get the size in bytes of each value in the current MRC format
	# print sizeof

	with open(fpath, 'rb+') as f:

		NX = header['dimensions'][2]
		NY = header['dimensions'][1]
		NZ = header['dimensions'][0]
		print 'Current dimensions of MRC file: NX=%d,NY=%d,NZ=%d' % (NX,NY,NZ)
		NZnew = ( S - hsize ) / ( NX*NY*sizeof ) # This will yield the actual number of Z slices
		print 'New dimensions of MRC file: NX=%d,NY=%d,NZ=%d' % (NX,NY,NZnew)
		header['dimensions'][0] = NZnew

		if options.stats:
		# Let's update the statistics of the stack/volume:
			header['meanImage'] = 0.0

			f.seek(1024)
			for i in np.arange(NZnew):

				# We will read one image or slice at a time in order to save memory:
				image = np.fromfile( f, dtype=header['dtype'], count=NX*NY )

				minImage = np.min(image)
				maxImage = np.max(image)
				meanImage = np.mean(image)

				if minImage < header['minImage']:
					header['minImage'] = minImage

				if maxImage > header['maxImage']:
					header['maxImage'] = maxImage

				header['meanImage'] += meanImage

			header['meanImage'] /= NZnew


		# Now we write the header back:
		ioMRC.writeMRCHeader( f, header, endchar )

if __name__ == "__main__":
	main()