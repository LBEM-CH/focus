from EMAN2 import *
from sparx import *

#import math
#from pylab import *
#from matplotlib import rc
#import numpy as np

import os



if __name__ == "__main__":
	
	if len(sys.argv) != 5:
		sys.exit("Missuse detected (apply filter")

	filename_in = sys.argv[1]
	freq = float(sys.argv[2])
	i = int(sys.argv[3])
        imagesidelength = int(sys.argv[4])
	
	print freq
	
	frame = get_image(filename_in)
	frame = filt_tophatl(frame, freq)
	frame.write_image(filename_in)
	
	if i != 1:
		w = get_image("weight.mrc")
		w += model_circle(imagesidelength*freq, imagesidelength, imagesidelength)
		w.write_image("weight.mrc")
	else:
		w = model_circle(imagesidelength*freq, imagesidelength, imagesidelength)
		w.write_image("weight.mrc")
		
	
