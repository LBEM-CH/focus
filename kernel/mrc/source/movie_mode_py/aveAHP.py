from EMAN2 import *
from sparx import *

import os


if __name__ == "__main__":
	
	if len(sys.argv) != 5:
		sys.exit("Missuseage detected, usage exe mode stating ending")

	image_name = sys.argv[1]
	mode = int(sys.argv[2])
	starting = int(sys.argv[3])
	ending = int(sys.argv[4])
	
	weight = [49, 27.56, 8.51, 4.17, 2.48, 1.65, 1.17, 0.25]
	
	data = []
	
	if mode==1:
		postfix = ".fou.limit.aph"
	else:
		postfix = ".fou.nolimit.aph"
	
	for i in range(starting, ending+1):
		data.append([])
		f = open("frames/frame_" + str(i) + "/APH/" + image_name + postfix)
		data[i-1] = f.readlines()
		f.close()
		
	data_out = []
	data_out.append(data[0][0])
	
	for i in range(1,len(data[0])):
		sum_A = 0
		sum_P = 0
		sum_B = 0
		sum_W = 0
		for j in range(starting-1, ending):
			line = data[j][i].split()
			if j==1:
				H = line[0]
				K = line[1]
				IQ = line[4]
				
			iq = int(line[4])
			if iq < 9:
				w = weight[iq-1]
			else:
				w = 0
			
			sum_A += w*float(line[2])
			sum_P += w*float(line[3])
			sum_B += w*float(line[5])
			sum_W += w
			
		if sum_W > 0:
			sum_A /= float(sum_W)	
			sum_P /= float(sum_W)	
			sum_B /= float(sum_W)
		else:
			sum_A = 0	
			sum_P = 0	
			sum_B = 0
		
		IQ = 1
		line_out = H + '\t' + K + '\t' + str(sum_A) + '\t' + str(sum_P) + '\t' + "1" + '\t' + str(sum_B) + '\t' + "0.0" + "\n"
		data_out.append(line_out)
		
	
	if mode==1:
		file_out = open("frame.limit.aph", "w")
	else:
		file_out = open("frame.nolimit.aph", "w")
	
	
	file_out.writelines(data_out)
	file_out.close
	
