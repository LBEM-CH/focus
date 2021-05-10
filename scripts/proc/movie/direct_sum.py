from EMAN2 import *
from sparx import *

import os


if __name__ == "__main__":
	
	if len(sys.argv) != 4:
		sys.exit("Missuse detected (direct_sum.py)")

	total_number = int(sys.argv[1])
	image_name = sys.argv[2]
	dir_frame_folder = sys.argv[3]
	
	file_name = dir_frame_folder + "/CCUNBEND_f1_notap.mrc"
	image = get_image(file_name)
	
	for i in range(2,total_number+1):		
		
		file_name = dir_frame_folder + "/CCUNBEND_f" + str(i) + "_notap.mrc"
		print ( i, file_name )
		
		image_tmp = get_image(file_name)
		image += image_tmp
			
	
	file_name_out = dir_frame_folder + "/direct_sum.mrc"
	print ( file_name_out )
	
	image /= total_number
	image.write_image(file_name_out)
