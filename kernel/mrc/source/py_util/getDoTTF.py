import sys

def getAbsTiltang(image_dir):
	config = image_dir + "/2dx_image.cfg"
	f = open(config)
	for line in f:
		if line.startswith("set TLTANG"):
			return abs(float(line.split('"')[1]))


if __name__ == "__main__":
	
	if len(sys.argv) != 3:
		sys.exit("Missuseage detected")


	min_angle = float(sys.argv[1])	
	image = sys.argv[2]
	
	#print "checking for TTF:" , image, "with min angle =", min_angle
	
	print (getAbsTiltang(image) > min_angle)
