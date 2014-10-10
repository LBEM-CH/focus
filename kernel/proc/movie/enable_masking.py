from EMAN2 import *
from sparx import *

import os


if __name__ == "__main__":
	
	if len(sys.argv) != 2:
		sys.exit("Missuseage detected")

	frame_folder = sys.argv[1]
	print "enabling masking for", frame_folder
	
	f = open(frame_folder + "2dx_image.cfg", 'r')
	lines = []
		
	for l in f:
		if l.startswith("set domask"):
			lines.append('set domask = "y"\n')
		else:
			lines.append(l)
	f.close()
	
	fout = open(frame_folder + "2dx_image.cfg", 'w')
	
	fout.writelines(lines)
	fout.close()
