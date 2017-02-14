# !/usr/bin/env python
#############################################################################
#                                                                           #
# Title:ignore duplicate entries from dataset								#
#                                                                           #
# (C) 2dx.org, GNU Plublic License.                                         #
#                                                                           #
# Created..........:13/02/2017                                            	#
# Last Modification:13/02/2017                                             	#
# Author...........:Ricardo Righetto                                       	#
#                                                                           #
#############################################################################

import numpy as np
import sys

def main():

	merge_dirfile = sys.argv[1]

	with open( merge_dirfile, 'r' ) as f:

		dirfile = f.readlines()

	imnames = []

	for d in dirfile:

		imnames.append( d.split('/')[-1].strip() )

	imunique,idx = np.unique(imnames, return_index=True)

	dirfile_unique = np.array(dirfile)[idx]

	if sys.argv[3] == 'y':
		ignore_lat2 = True
	else:
		ignore_lat2 = False

	if ignore_lat2:

		lat2 = np.core.defchararray.find(dirfile_unique,'_lat2')
		dirfile_unique = dirfile_unique[lat2 == -1]

	if sys.argv[4] == 'y':
		ignore_c1 = True
	else:
		ignore_c1 = False

	if ignore_c1:

		c1 = np.core.defchararray.find(dirfile_unique,'_c1')
		dirfile_unique = dirfile_unique[c1 == -1]

	np.savetxt( sys.argv[2], np.core.defchararray.strip(dirfile_unique), fmt='%s' )

if __name__ == '__main__':
	main()