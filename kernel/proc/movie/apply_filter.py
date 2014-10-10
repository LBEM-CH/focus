from EMAN2 import *
from sparx import *

#import math
#from pylab import *
#from matplotlib import rc
#import numpy as np

import os



if __name__ == "__main__":
	
	if len(sys.argv) != 4:
		sys.exit("Missuseage detected (apply filter")

	filename_in = sys.argv[1]
	freq = float(sys.argv[2])
	i = int(sys.argv[3])
	
	print freq
	
	frame = get_image(filename_in)
	frame = filt_tophatl(frame, freq)
	frame.write_image(filename_in)
	
	if i != 1:
		w = get_image("weight.mrc")
		w += model_circle(3808*freq, 3808, 3808)
		w.write_image("weight.mrc")
	else:
		w = model_circle(3808*freq, 3808, 3808)
		w.write_image("weight.mrc")
		
	
