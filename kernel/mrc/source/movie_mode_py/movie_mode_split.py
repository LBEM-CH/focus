from EMAN2 import *
from sparx import *


if __name__ == "__main__":
	
	if len(sys.argv) != 2:
		sys.exit("Missuseage detected")

	stack_file = sys.argv[1]
	
	print "Doing splitting for: ", stack_file
