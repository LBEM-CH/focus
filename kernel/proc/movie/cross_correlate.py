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
	filename_ref = sys.argv[2]
	filename_out = sys.argv[3]
	
	image = get_image(filename_in)
	reference = get_image(filename_ref)

	outimage = ccf(image,reference,center=True)

	outimage.write_image(filename_out)
			
	
