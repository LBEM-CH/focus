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
	refdiameter = sys.argv[3]

	# We want to use a circle with the same surface as a square with "refdiameter" width:
	# 1.273 = 4 / PI
	# The radius is then half that:
        refradius = 0.6366 * float(refdiameter)
	
	image = get_image(filename_in)
        imageinfo = info(image)
        imagesize = imageinfo[4]
        imageave = imageinfo[0]
        print "Read image ", filename_in, ", size = ", imagesize, ", average = ",imageave

        mask = model_circle(refradius,imagesize,imagesize)
 
        image -= imageave
        image *= mask
        image += imageave 

	image.write_image(filename_out)
			
	
