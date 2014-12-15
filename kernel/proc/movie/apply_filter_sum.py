from EMAN2 import *
from sparx import *


import os

if __name__ == "__main__":
	
	if len(sys.argv) != 3:
		sys.exit("Missuse detected (apply_filter_sum.py)")

	filename_in = sys.argv[1]
	filename_out = sys.argv[2]
		
	frame = get_image(filename_in)
	
	w = get_image("weight.mrc")
	
	frame_f = fft(frame)
	B = frame_f.filter_by_image(1.0/(w+0.001))
	

	# o = filt_tophatl(B, 0.48)
	# o.write_image(filename_out)
	B.write_image(filename_out)
	
