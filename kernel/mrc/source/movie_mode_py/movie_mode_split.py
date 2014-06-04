from EMAN2 import *
from sparx import *

import os


if __name__ == "__main__":
	
	if len(sys.argv) != 4:
		sys.exit("Missuseage detected")

	stack_file = sys.argv[1]
	image_name = sys.argv[2]
	ave_num = int(sys.argv[3])
	
	stack = get_image(stack_file)
	nx = stack.get_xsize()
	ny = stack.get_ysize()
	nz = stack.get_zsize()
	
	print "Doing splitting for:", stack_file
	print "nz =", nz
	
	for i in range(0,int(nz/ave_num)):
		
		folder_name = "frames/frame_" + str(i+1)
		output_name = folder_name + "/" + image_name + "_" + str(i+1) + ".mrc"
		os.mkdir(folder_name)
		
		for j in range(0,ave_num):
		
			print "Splitting frame", i+1, "into", output_name, j
			
			if j==0:
				image = stack.get_clip(Region(0,0,i*ave_num+j,nx,ny,1))
			else:
				image += stack.get_clip(Region(0,0,i*ave_num+j,nx,ny,1))
			
		image.write_image(output_name)
	
	
