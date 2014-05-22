from EMAN2 import *
from sparx import *

import os


def compare_peaks(a,b):
	r1 = (a[0]-x)**2 + (a[1]-y)**2
	r2 = (b[0]-x)**2 + (b[1]-y)**2
	
	if r1>r2:
		return 1
	elif r1==r2:
		return 0
	else:
		return -1

if __name__ == "__main__":
	
	if len(sys.argv) != 7:
		sys.exit("Missuseage detected")

	infile = sys.argv[1]
	outfile = sys.argv[2]
	
	x = int(sys.argv[3])
	y = int(sys.argv[4])
	n = int(sys.argv[5])
	
	outfile2 = sys.argv[6]
	
	f = open(infile)
	peaks = []	
		
	for l in f:
		tmp = l.split()
		tmp2 = []
		for t in tmp:
			tmp2.append(float(t))
		peaks.append(tmp2)
	f.close()
	
	peaks.sort(compare_peaks)
	
	fout = open(outfile, "w")
	fout2 = open(outfile2, "a")
	
	lines = []
	for p in peaks[:n]:
		l = str(p[0]) + " " + str(p[1]) + "\n"
		lines.append(l)
		
	
	fout.writelines(lines)
	fout.close()
	
	fout2.writelines(lines)
	fout2.close()
