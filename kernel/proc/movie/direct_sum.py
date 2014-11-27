from EMAN2 import *
from sparx import *

import os


if __name__ == "__main__":
	
	if len(sys.argv) != 3:
		sys.exit("Missuse detected (direct_sum.py)")

	total_number = int(sys.argv[1])
	image_name = sys.argv[2]
	
	file_name = "frames/CCUNBEND_frame_1.notap.mrc"
	image = get_image(file_name)
	
	for i in range(2,total_number+1):		
		
		file_name = "frames/CCUNBEND_frame_" + str(i) + ".notap.mrc"
		print i, file_name
		
		image_tmp = get_image(file_name)
		image += image_tmp
			
	
	file_name_out = "frames/direct_sum.mrc"
	print file_name_out
	
	image /= total_number
	image.write_image(file_name_out)
