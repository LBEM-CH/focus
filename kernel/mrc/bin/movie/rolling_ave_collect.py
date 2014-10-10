from EMAN2 import *
from sparx import *

import os


if __name__ == "__main__":
	
	if len(sys.argv) != 4:
		sys.exit("Missuseage detected")

	total_number = int(sys.argv[1])
	offset = int(sys.argv[2])
	out_name = sys.argv[3]
	
	data = []
	
	#for i in range(offset/2+1, total_number-offset/2+1):		
	for i in range(offset, total_number, offset):	
		
		file_name = "frames/rolling_aves/ave_" + str(i) + ".fou.nolimit.aph"
		print i, file_name
		
		file_in = open(file_name, 'r')
		file_in.readline()
		
		if i == offset:
			count = 0
			for l in file_in:
				tmp = l.split()
				data.append([int(tmp[0])])
				data[count].append(int(tmp[1]))
				data[count].append(float(tmp[2]))
				count += 1
		else:
			count = 0
			for l in file_in:
				tmp = l.split()
				data[count].append(float(tmp[2]))
				count += 1
				
		file_in.close()
				
	file_out = open(out_name, 'w')
	
	for d in data:
		file_out.write(str(d)[1:-1] + "\n")
	file_out.close()
