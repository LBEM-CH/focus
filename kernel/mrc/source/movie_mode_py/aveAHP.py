from EMAN2 import *
from sparx import *

import math

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
	
	drad = 3.141592654/180.0
	
	for i in range(1,len(data[0])):
		sum_A = 0.0
		sum_Aw = 0.0
		sum_sin = 0.0
		sum_cos = 0.0
		sum_P = 0.0
		sum_Back = 0.0
		counter = 0.0
		for j in range(starting-1, ending):
			line = data[j][i].split()
			H = line[0]
			K = line[1]
				
			iq = int(line[4])
			if iq < 9:
				w = weight[iq-1]
			else:
				w = 0
			
			back = float(line[5])
			
			
			amp = float(line[2])
			r = sqrt(float(H)**2 + float(K)**2)
			amp = amp * exp(-0.12*r)
			
			if amp>0.0001:
				sum_A += amp * amp/back
				sum_Aw += amp/back
				sum_Back += 1.0 / back
				counter += 1
				
				ang = drad * float(line[3])
				cosang = cos(ang)
				sinang = sin(ang)
				
				w = amp/back
				
				sum_sin += w * sinang
				sum_cos += w * cosang
				sum_P += w
			
			
			
		if sum_A > 0.0:
			AMP = sum_A / sum_Aw
		else:
			AMP = 0
			
		AVRGANG = math.atan2(sum_sin, sum_cos)
		COMBPHASE = AVRGANG / drad

		if counter>0:
			tmp = sum_Back / sqrt(float(counter))
			BACK = 1.0 / tmp
		else:
			BACK = 0.0
		
		if AMP < 0.001:
			IQ = 9
		else:
			PHSERR = (BACK/AMP)
			IQ = 1 + int(7*PHSERR)
			IQ = min(IQ,8)
		
		line_out = H + '\t' + K + '\t' + str(round(AMP,1)) + '\t' + str(round(COMBPHASE)) + '\t' + str(IQ) + '\t' + str(round(BACK,1)) + '\t' + "0.0" + "\n"
		data_out.append(line_out)
		
	
	if mode==1:
		file_out = open("frame.limit.aph", "w")
	else:
		file_out = open("frame.nolimit.aph", "w")
	
	
	file_out.writelines(data_out)
	file_out.close
	
