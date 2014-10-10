import os
import sys

if __name__ == "__main__":
	
	if len(sys.argv) != 2:
		sys.exit("Missuseage detected")

	frame_folder = sys.argv[1]
	
	f = open(frame_folder + "/2dx_image.cfg", 'r')
	lines = []
		
	for l in f:
		if l.startswith("set imagename_original"):
			tmp = l.split()
			tmp2 = tmp[3].split('"')
			print tmp2[1]
			sys.exit(0)
	f.close()
	
	print "invalid"
