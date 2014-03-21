from EMAN2 import *
from sparx import *

import os


if __name__ == "__main__":
	
	if len(sys.argv) != 4:
		sys.exit("Missuseage detected")

	total_number = int(sys.argv[1])
	offset = int(sys.argv[2])
	image_name = sys.argv[3]
	
	#stack = get_image(stack_file)
	#nx = stack.get_xsize()
	#ny = stack.get_ysize()
	#nz = stack.get_zsize()
	
	#print "Doing splitting for:", stack_file
	#print "nz =", nz
	
	for i in range(offset/2+1, total_number-offset/2+1):		
		
		#image = get_image()
		
		file_name = "frames/frame_" + str(i) + "/SCRATCH/corm" + image_name + ".notap.mrc"
		print i, file_name
		
		image = get_image(file_name)
		
		for j in range(-offset/2+1, offset/2+1):
			if j==0:
				continue
				
			file_name = "frames/frame_" + str(i+j) + "/SCRATCH/corm" + image_name + ".notap.mrc"
			print "\t", i+j, file_name
			
			image_tmp = get_image(file_name)
			image += image_tmp
			
	
		file_name_out = "frames/rolling_aves/ave_" + str(i) + ".mrc"
		print file_name_out
		
		image /= offset
		image.write_image(file_name_out)
