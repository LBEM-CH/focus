from EMAN2 import *
from sparx import *

import os


if __name__ == "__main__":
	
	if len(sys.argv) != 2:
		sys.exit("Missuse detected")

	frame_folder = sys.argv[1]
	print "disabling movie_mode for", frame_folder
	
	f = open(frame_folder + "2dx_image.cfg", 'r')
	lines = []
		
	for l in f:
		if l.startswith("set movie_enable"):
			lines.append('set movie_enable = "n"\n')	
		elif l.startswith("set crop_histogram = "):
			lines.append('set crop_histogram = "y"\n')
		elif l.startswith("set crop_histogram_percent = "):
			lines.append('set crop_histogram_percent = "2"\n')
		elif l.startswith("set crop_histogram_stdev = "):
			lines.append('set crop_histogram_stdev = "4"\n')
		else:
			lines.append(l)
	f.close()
	
	fout = open(frame_folder + "2dx_image.cfg", 'w')
	
	fout.writelines(lines)
	fout.close()
