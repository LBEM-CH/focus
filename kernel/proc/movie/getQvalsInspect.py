import sys


if __name__ == "__main__":
	
	if len(sys.argv) != 3:
		sys.exit("Missuseage detected")


	filename = sys.argv[1]	
	config_file = sys.argv[2]
	
	f = open(filename)
	for line in f:
		if line.startswith("set QVALS"):
			qvalS = float(line.split()[3])
			break
	f.close()
	
	f2 = open(config_file)
	for line in f2:	
		if line.startswith("set QVAL2"):
			qval2 = float(line.split('"')[1])
			continue
		if line.startswith("set TLTANG"):
			ang = float(line.split('"')[1])
			continue
	
	if (qvalS/qval2)<=1:
		print ":: \tRevisit", filename.split("/")[-1]
	
	print qval2, qvalS, round(qvalS/qval2,2), abs(ang)
