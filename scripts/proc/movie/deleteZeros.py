from EMAN2 import *
from sparx import *

import os


if __name__ == "__main__":
	
	if len(sys.argv) != 3:
		sys.exit("Missuseage detected")

	infile = sys.argv[1]
	outfile = sys.argv[2]
	
	
	f = open(infile)
	lines = []
	
	for i in range(0,7):
		f.readline()
		
	for l in f:
		if (l.split()[0]).startswith("0"):
			continue
		else:
			lines.append(l)
	f.close()
	
	fout = open(outfile, "w")
	
	fout.writelines(lines)
	fout.close()
