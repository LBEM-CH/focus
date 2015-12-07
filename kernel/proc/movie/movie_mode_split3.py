from EMAN2 import *
from sparx import *

import os


if __name__ == "__main__":
	
	if len(sys.argv) != 5:
		sys.exit("Missuse detected")

	stack_file = sys.argv[1]
	image_name = sys.argv[2]
	folder_name = sys.argv[3]
	skip_num = int(sys.argv[4])
	
	stack = get_image(stack_file)
	nx = stack.get_xsize()
	ny = stack.get_ysize()
	nz = stack.get_zsize()
	
	print ":Doing splitting for:", stack_file
	print ":nz =", nz
	


        #########################################
        # create frames 1 to nz+1:
        #########################################

	for i in range(0,nz-skip_num):
						
		output_name = folder_name + "/frame_" + str(i+1) + ".mrc"
				
		print "Creating frame", i+1, "as", output_name
		
                ioffx = 0
                ioffy = 0
		
		# The following is to test the drift correction performance:
		# ioffx = i/2-9
		# ioffy = i-19

		image = stack.get_clip(Region(ioffx,ioffy,i+skip_num,nx,ny,1))

		image.write_image(output_name)
	
	
