from EMAN2 import *
from sparx import *

import os

def w(x, t, a, b):
	if t==0:
		return a*exp(b*x)
	else:
		return 0.5

def getCutoff(n, t, a, b):
	return min(0.5, w(n, t, a, b))

if __name__ == "__main__":
	
	# Usage getFilter.py n type a b
	
	if len(sys.argv) != 5:
		sys.exit("Missuseage detected filter")

	number = int(sys.argv[1])
	ftype = int(sys.argv[2])
	a = float(sys.argv[3])
	b = float(sys.argv[4])
	
	
	print getCutoff(number, ftype, a, b)

	
