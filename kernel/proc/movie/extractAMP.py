

import math

import os
import sys

def getResolution(h,k):
        # This is really badly case specific here.....:
	spacing = 39.1
	xh = h * spacing
	xk = k * spacing
	d = math.sqrt(xh**2 + xk**2)
	r = d/3808.0
	return 1.34/r
	
def myComp(a,b):
	return float(a.split()[0]) < float(b.split()[0])

def getKey(item):
	return float(item.split()[0])
	

if __name__ == "__main__":
	
	if len(sys.argv) < 3:
		sys.exit("Missuse detected, usage exe mode stating ending")

	image_names = sys.argv[1:]
	
	print "doing AMP extraction on", image_names
	
	amp_files = []
	for name in image_names:
		amp_files.append(open(name))
		
	for f in amp_files:
		f.readline()
		
	results = []

	for l in amp_files[0]:
		valid_bit = True
		iq = int(l.split()[4])
		amp = float(l.split()[2])
		res = ""
		if amp>0:
			res += str(getResolution(float(l.split()[0]),float(l.split()[1]))) + " " + str(float(l.split()[2])/amp) 
		for f in amp_files[1:]:
			line = f.readline()
			if amp>0:
				res += " " + str(float(line.split()[2])/amp)
			if (float(line.split()[2])<=0) and (valid_bit==True):
				valid_bit = False
		res += "\n"
		if (iq <= 7) and valid_bit==True:
			results.append(res) 
		
		
#	for l1, l2, l3, l4 in zip(amp_files[0], amp_files[1], amp_files[2], amp_files[3]):	
#		if (l1.split()[0] == l2.split()[0] == l3.split()[0] == l4.split()[0]) and (l1.split()[1] == l2.split()[1] == l3.split()[1] == l4.split()[1]): 
#			iq = int(l1.split()[4])
#			if iq <= 7 and float(l1.split()[2])>0 and float(l2.split()[2])>0 and float(l3.split()[2])>0 and float(l4.split()[2])>0:
#				amp = float(l1.split()[2])
#				frac1 = float(l1.split()[2]) / amp
#				frac2 = float(l2.split()[2]) / amp
#				frac3 = float(l3.split()[2]) / amp
#				frac4 = float(l4.split()[2]) / amp
#				res = str(getResolution(float(l1.split()[0]),float(l1.split()[1]))) + " " + str(frac1) + " " + str(frac2) + " " + str(frac3) + " " + str(frac4)  + "\n"
#				results.append(res)
				
	for f in amp_files:
		f.close()
				
	out_file = open("frames/AMPs.txt", "w")
	out_file.writelines(sorted(results, key=getKey))
	out_file.close()
