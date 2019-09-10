from EMAN2 import *
from sparx import *

#import math
#from pylab import *
#from matplotlib import rc
#import numpy as np

import os



if __name__ == "__main__":
	
	if len(sys.argv) != 4:
		sys.exit("Missuse detected (mask)")

	filename_in = sys.argv[1]
	filename_out = sys.argv[2]
	maskinginfo = sys.argv[3]
	
	frame = get_image(filename_in)
	frameinfo = info(frame)
	framesize = frameinfo[4]
	print( "Read image ", filename_in, ", size = ", framesize )

	maskfile = get_image(maskinginfo)
	maskinfo = info(maskfile)
	masksize = maskinfo[4]
	print( "Read image ", maskinginfo, ", size = ", masksize )

	r = 1.0 * framesize / masksize
	print( "Upscaling by a factor of %10.6f" % r )

	mask = resample(maskfile,r)
	maskinfo = info(mask)
	maskmax  = maskinfo[3]
	masksize = maskinfo[4]
	print( "New masking file ", maskinginfo, " has size = ", masksize )
	if maskmax < 1.0:
		maskmax = 1.0

	s = info(frame,mask)
	average = s[0]

	frame -= average
	frame *= mask / maskmax
	frame += average

	frame.write_image(filename_out)
			
	
