from EMAN2 import *
from sparx import *

import math
from pylab import *
from matplotlib import rc
import numpy as np

import os

def fit(data):
	x = []
	y = []
	
	count = 1
	tmp_old = 10000000
	for d in data:
		tmp = float(d)
		
		if tmp > 0.0:
			y.append(math.log(tmp**2))
			x.append(count)
		count += 1
		
	if len(y) < 0.5:
		return 0
		
	X = array(x)
	Y = array(y)
	
	slope,intercept = np.polyfit(X,Y,1)
	
	#print intercept
	
	res = norm((slope*X + intercept) - Y)
	print res
	
	
	if abs(res)>50:
		return 0
	
	#print -1.0/slope
	return -1.0/slope
		
	

if __name__ == "__main__":
	
	if len(sys.argv) != 3:
		sys.exit("Missuseage detected")

	filename_in = sys.argv[1]
	filename_out = sys.argv[2]
	
	
	filein = open(filename_in)
	
	r = []
	ry = []
	for l in filein:
		tmp = l.split(", ")
		
		value = fit(tmp[2:])
			
		if value > 0 and value<20:
			r.append(sqrt(float(tmp[0])**2 + float(tmp[1])**2))
			ry.append(value)
		
	dr = 5
	rmin = 5
	rmax = 60
	
	xbin = []
	ybin = []
	sdbin = []
	
	while rmin < rmax:
		val_sum = []
		counter = 0
		for i in range(1,len(r)):
			if r[i]>rmin and (r[i]>(rmin+dr)):
				val_sum.append(ry[i])
		xbin.append(rmin)
		ybin.append(np.median(val_sum))
		sdbin.append(np.std(val_sum))
		rmin += dr
		
	for i in range(1,len(xbin)):
		print xbin[i], ybin[i], sdbin[i]
		
	
	figure()
	ax = subplot(1,1,1)
	ax.plot(r,ry,'.')
	
	#ax.errorbar(xbin, ybin, yerr=sdbin, fmt='o')
	
	plt.savefig("test.pdf")
		
		
	Xm = array([xbin])
	Ym = log(array([ybin]))
	w = linalg.lstsq(Xm.T,Ym.T)[0]
		
	print w[0]
