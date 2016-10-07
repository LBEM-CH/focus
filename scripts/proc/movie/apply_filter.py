from EMAN2 import *
from sparx import *

#import math
#from pylab import *
#from matplotlib import rc
#import numpy as np

import os



if __name__ == "__main__":
	
	if len(sys.argv) != 6:
		sys.exit("Missuse detected (apply filter)")

	filename_in = sys.argv[1]
	freq = float(sys.argv[2])
	i = int(sys.argv[3])
	imagesidelength = int(sys.argv[4])
	filename_weight = sys.argv[5]
	
	sigma = imagesidelength/2.0 * freq
	# print ": imagesidelength", imagesidelength,"     Frequency = ", freq, "      Sigma = ", sigma

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

	# s = info(frame)
	# nx = s[4]
	# ny = s[5]	
	# print ":: Size = ", nx, ",", ny

	if i > 0:
		if i == 1:
			w.write_image(filename_weight)
		else:
			g = get_image(filename_weight)
			g += w
			g.write_image(filename_weight)
			
	
