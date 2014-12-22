from EMAN2 import *
from sparx import *

import os


if __name__ == "__main__":
	
	if len(sys.argv) != 3:
		sys.exit("Missuse detected")

	stack_file = sys.argv[1]
	image_name = sys.argv[2]
	
	stack = get_image(stack_file)
	nx = stack.get_xsize()
	ny = stack.get_ysize()
	nz = stack.get_zsize()
	
	print ":Doing splitting for:", stack_file
	print ":nz =", nz
	


        #########################################
        # create frames 1 to nz+1:
        #########################################

	for i in range(0,nz):
		
		output_name = "frames/frame_" + str(i+1) + ".mrc"
				
		print "Creating frame", i+1, "as", output_name
		
                ioffx = 0
                ioffy = 0
		
		# The following is to test the drift correction performance:
		# ioffx = i/2
		# ioffy = i

		image = stack.get_clip(Region(ioffx,ioffy,i,nx,ny,1))

		image.write_image(output_name)
	
	
