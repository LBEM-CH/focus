from EMAN2 import *
from sparx import *

import os


if __name__ == "__main__":
	
	if len(sys.argv) != 2:
		sys.exit("Missuse detected (direct_sum2.py)")

	total_number = int(sys.argv[1])
	
	file_name = "frames/CCmap_1_unbent.mrc"
	image = get_image(file_name)
	
	for i in range(2,total_number+1):		
		
		file_name = "frames/CCmap_" + str(i) + "_unbent.mrc"
		print i, file_name
		
		image_tmp = get_image(file_name)
		image += image_tmp
			
	
	file_name_out = "frames/CCmap_driftCorrected.mrc"
	print file_name_out
	
	image /= total_number
	image.write_image(file_name_out)
