from EMAN2 import *
from sparx import *

import os

def w(x):
	return 0.7559*exp(-0.06881*x)

def getCutoff(n):
	return min(0.5, w(n))

if __name__ == "__main__":
	
	if len(sys.argv) != 2:
		sys.exit("Missuseage detected filter")

	number = int(sys.argv[1])
	
	print getCutoff(number)

	
