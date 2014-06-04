import os
import sys
import math
import numpy as np
from pylab import *
from matplotlib import rc


def getProfStat(filename):
	
	profile = open(filename, 'r')

	for i in range(0,7):
		profile.readline()
			
	peak_vec = []
	
	for l in profile:
		data = l.split()
		if float(data[0])>0 and float(data[1])>0:
			peak = float(data[0]), float(data[1]), float(data[2])
			peak_vec.append(peak)

	R = 93
	minR = 30
	maxR = 140
	
	Rvec = []
	
	for d1 in peak_vec:
		for d2 in peak_vec:
			current_r = math.sqrt((d1[0]-d2[0])**2 + (d1[1]-d2[1])**2)
			if (current_r > minR) and (current_r < maxR):
				Rvec.append(current_r)
				
	return np.mean(Rvec), np.std(Rvec)
	

if __name__ == "__main__":
	
	if len(sys.argv) != 4:
		sys.exit("Missuseage detected")
		
	first = int(sys.argv[1])
	last = int(sys.argv[2])
	image_name = sys.argv[3]
	
	means = []
	sds = []
	for i in range(first, last+1):
		result = getProfStat("frames/frame_" + str(i) + "/SCRATCH/profm" + image_name +".dat")
		means.append(result[0])
		sds.append(result[1])
		
	figure()
	ax = subplot(1,1,1)
	ax.plot(range(first,last+1), sds)
	plt.title('SD of CC-peak locations')
	plt.xlabel('Frame number')
	plt.ylabel('SD of cc-peaks')
	
	
	plt.savefig("frames/sd.pdf")

	
