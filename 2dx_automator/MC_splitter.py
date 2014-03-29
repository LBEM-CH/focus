#!/usr/bin/python


from EMAN2 import *
from sparx import *

import os

def extactImage(name, output, first, last):
	print "Extracting average for:", name
	
	stack = get_image(name)
	nx = stack.get_xsize()
	ny = stack.get_ysize()
	nz = stack.get_zsize()
	
	print "\tNumber of frames:", nz
	
	if nz < last:
		print "\tEctraction failed: last>nz"
		return
		
	image = stack.get_clip(Region(0,0,0,nx,ny,1))
	
	print "\t\tRead frame number:\t1"
	for i in range(1,last):
		print "\t\tAdding frame number:\t", i+1
		image_new = stack.get_clip(Region(0,0,i,nx,ny,1))
		image += image_new
	
	tmp = name.split("/")[-1]	
	corename = tmp[:-4]
	
	outname = output + "/" + corename + "_#" + str(first) + "_#" + str(last) + ".mrc"
	print "\tStoring average to:", outname
	image.write_image(outname)
	

if __name__ == "__main__":
	
	if len(sys.argv) != 5:
		sys.exit("Usage infolder outfolder starting_frame ending_frame")

	infolder = sys.argv[1]
	outfolder = sys.argv[2]
	first_frame = int(sys.argv[3])
	last_frame = int(sys.argv[4])
	
	for s in os.listdir(infolder):
		stack_name = infolder + "/" + s
		if os.path.getsize(stack_name) > 1000000:
			extactImage(stack_name, outfolder, first_frame, last_frame)
	
	
	
