from EMAN2 import *
from sparx import *

import os


if __name__ == "__main__":
	
	if len(sys.argv) != 2:
		sys.exit("Missuseage detected")

	frame_number = sys.argv[1]
	
	f = open("frames/frame_" + frame_number + "/2dx_image.cfg", 'r')
		
	for l in f:
		if l.startswith("set QVAL2"):
			qval = (l.split()[3]).split('"')[1]
			break
	f.close()
	
	f_out = open("movie_qvals.txt", "a")
	line_out = frame_number + "\t" + qval + "\n"
	f_out.write(line_out)
	f_out.close()
