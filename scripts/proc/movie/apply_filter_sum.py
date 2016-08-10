from EMAN2 import *
from sparx import *


import os

if __name__ == "__main__":
	
	if len(sys.argv) != 5:
		sys.exit("Missuse detected (apply_filter_sum.py)")

	filename_in = sys.argv[1]
	filename_out = sys.argv[2]
	noise = float(sys.argv[3])
	noise2 = noise * noise
	filename_weight = sys.argv[4]
		
	frame = get_image(filename_in)
	
	w = get_image(filename_weight)
	
	frame_f = fft(frame)
	B = frame_f.filter_by_image(1.0/(w+noise2))
	

	# o = filt_tophatl(B, 0.48)
	# o.write_image(filename_out)

	B.write_image(filename_out)
	
