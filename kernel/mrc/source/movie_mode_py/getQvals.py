import sys


if __name__ == "__main__":
	
	if len(sys.argv) != 2:
		sys.exit("Missuseage detected")


	filename = sys.argv[1]	
	
	f = open(filename)
	for line in f:
		if line.startswith("set QVAL2"):
			qval2 = float(line.split('"')[1])
			continue
		if line.startswith("set QVALS"):
			qvalS = float(line.split('"')[1])
			continue
		if line.startswith("set TLTANG"):
			ang = float(line.split('"')[1])
			continue
	
	print qval2, qvalS, round(qvalS/qval2,2), abs(ang)
