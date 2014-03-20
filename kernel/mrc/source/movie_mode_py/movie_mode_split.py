from EMAN2 import *
from sparx import *

import os


if __name__ == "__main__":
	
	if len(sys.argv) != 3:
		sys.exit("Missuseage detected")

	stack_file = sys.argv[1]
	image_name = sys.argv[2]
	
	stack = get_image(stack_file)
	nx = stack.get_xsize()
	ny = stack.get_ysize()
	nz = stack.get_zsize()
	
	print "Doing splitting for:", stack_file
	print "nz =", nz
	
	for i in range(0,nz):		
		folder_name = "frames/frame_" + str(i+1)
		output_name = folder_name + "/" + image_name + "_" + str(i+1) + ".mrc"
		
		print "Splitting frame", i+1, "into", output_name
		
		os.mkdir(folder_name)   
		image = stack.get_clip(Region(0,0,i,nx,ny,1))
		image.write_image(output_name)
	
	
