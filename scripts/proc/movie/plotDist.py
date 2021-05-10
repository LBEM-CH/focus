from EMAN2 import *
from sparx import *

import math
from pylab import *
from matplotlib import rc
import os


from numpy import arange,array,ones,linalg

if __name__ == "__main__":
	
	if len(sys.argv) != 3:
		sys.exit("Missuseage detected, usage exe mode stating ending")

	infile = sys.argv[1]
	outfile = sys.argv[2]
	
	
	x = []
	y1 = []
	y2 = []

	f = open(infile)
	f.readline()
	for l in f:
		tmp = l.split()
		H = float(tmp[0])
		K = float(tmp[1])
		A = float(tmp[2])
		B = float(tmp[5])
		
		
		r = sqrt(H**2 + K**2)
		A = A * exp(-0.12*r)
		
		if A > 0.001:
			r = sqrt(H**2 + K**2)
			x.append(r)
			y1.append(A)
			y2.append(A/B)
		
	
	amax = max(y1)
	y1_2 = []
	for item in y1:
		y1_2.append(item/amax)
		
	figure()
	ax = subplot(1,1,1)
	ax.plot(x,y1_2,'.')
	ax.set_ylabel('A')
	
	Xm = array([x])
	Y = log(array([y1_2]))
	
	w = linalg.lstsq(Xm.T,Y.T)[0]
	print ( w[0] )
	
	y_fit = []
	for item in x:
		y_fit.append(exp(w[0][0] * item))
	
	ax2 = ax.twinx()
	ax2.plot(x, y_fit, 'r.')
	
	plt.savefig(outfile)
