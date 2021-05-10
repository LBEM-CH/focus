from EMAN2 import *
from sparx import *

#import math
#from pylab import *
#from matplotlib import rc
#import numpy as np

import os



if __name__ == "__main__":
	
	if len(sys.argv) != 6:
		sys.exit("Missuse detected (align and filter")

	filename_in = sys.argv[1]
	filename_average = sys.argv[2]
	freq = float(sys.argv[3])
	i = int(sys.argv[4])
        imagesidelength = int(sys.argv[5])
	
	print ( freq )
	
	frame = get_image(filename_in)
	avera = get_image(filename_average)

        ccf   = ccfpl(frame,avera,True)
        # ccf   = mask_circular(ccf,radius=100)
	ccf.write_image("ccf.mrc")
        
        # dx,dy = central_peak(ccf)

        # frame = cyclic_shift(frame,dx,dy,0)

        # e = img.copy()
        # Util.cyclicshift(e,{"dx":dx,"dy":dy,"dz":dz})
        # return e

	frame = filt_tophatl(frame, freq)

	frame.write_image(filename_in)
	
	if i != 1:
		w = get_image("weight.mrc")
		w += model_circle(imagesidelength*freq, imagesidelength, imagesidelength)
		w.write_image("weight.mrc")
	else:
		w = model_circle(imagesidelength*freq, imagesidelength, imagesidelength)
		w.write_image("weight.mrc")
		
	
