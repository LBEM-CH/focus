# !/usr/bin/env python
#############################################################################
#                                                                           #
# Title:Correct particle indices in .par file								#
#                                                                           #
# (C) 2dx.org, GNU Plublic License.                                         #
#                                                                           #
# Created..........:29/07/2016                                             #
# Last Modification:29/07/2016                                             #
# Author...........:Ricardo Righetto                                       #
#                                                                           #
#############################################################################
# After running the particle picker in parallel, the indices of the particles in the .par file will be messed up. This script will assign correct indices to them.

import numpy as np
import sys

def main():

	parfile = sys.argv[1]

	par = np.loadtxt(parfile, comments='C')

	N = par.shape[0]
	par[:,0] = np.reshape(np.arange(1,N+1),(1,N))

	# np.savetxt(parfile, par, fmt=['%d    %.2f    %.2f    %.2f    %.2f    %.2f    %d    %d    %.2f    %.2f    %.2f    %.2f    %d    %.4f    %.2f    %.2f'], delimiter='    ')
	np.savetxt(parfile, par, fmt='    %d    %.2f    %.2f    %.2f    %.2f    %.2f    %d    %d    %.2f    %.2f    %.2f    %.2f    %d    %.4f    %.2f    %.2f')

if __name__ == '__main__':
	main()