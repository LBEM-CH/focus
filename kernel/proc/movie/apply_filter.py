from EMAN2 import *
from sparx import *

#import math
#from pylab import *
#from matplotlib import rc
#import numpy as np

import os



if __name__ == "__main__":
	
	if len(sys.argv) != 5:
		sys.exit("Missuse detected (apply filter)")

	filename_in = sys.argv[1]
	freq = float(sys.argv[2])
	i = int(sys.argv[3])
        imagesidelength = int(sys.argv[4])
	
	sigma = imagesidelength/2.0 * freq
	# print ":     Frequency = ", freq, "      Sigma = ", sigma

	w = 100.0 * sigma * model_gauss(sigma, imagesidelength, imagesidelength)
	cen = Util.window(w,5,5,1,0,0,0)
	s = info(cen)
	max = s[3]
	if max > 0:
		w /= max
	
	frame = get_image(filename_in)

	frame_f = fft(frame)

	frame = frame_f.filter_by_image(w)

	# B = frame_f.filter_by_image(w)
	# frame = filt_tophatl(B, 0.48)

	frame.write_image(filename_in)
		
	if i == 1:
		w.write_image("weight.mrc")
	else:
		g = get_image("weight.mrc")
		g += w
		g.write_image("weight.mrc")
		
	
