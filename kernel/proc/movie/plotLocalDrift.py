import sys
import os

import matplotlib.pyplot as plt
from matplotlib.pyplot import figure, show
import numpy as np


def getTraj(name, n):
	f = open(name)
	
	x = []	
	y = []
	for l in f:
		tmp = l.split()
		x.append(float(tmp[0]))
		y.append(float(tmp[1]))
	f.close()
	
	n_loc = len(x) / n
	
	x_vec = []
	y_vec = []
	
	x_tmp = []
	y_tmp = []
	
	count = 0
	
	for i in range(0,n):
		x_tmp = []
		y_tmp = []
		for j in range(i,len(x),n):
			x_tmp.append(x[j])
			y_tmp.append(y[j])
		x_vec.append(x_tmp)
		y_vec.append(y_tmp)
		
	
	for i in range(0,len(x_vec)):
		ref = x_vec[i][0]
		for j in range(0,len(x_vec[i])):
			x_vec[i][j] -= ref
			
	for i in range(0,len(y_vec)):
		ref = y_vec[i][0]
		for j in range(0,len(y_vec[i])):
			y_vec[i][j] -= ref
			
	#print x_vec, y_vec
	
	vec_a = []
	vec_b = []
	
	for x,y in zip(x_vec,y_vec):
		if (max(x)<50) and (max(y)<50) and (min(x)>-50) and (min(y)>-50):
			vec_a.append(np.array(x))
			vec_b.append(np.array(y))
			
	print len(vec_a)
		
	mean_x = np.zeros(n_loc)
	for val in vec_a:
		mean_x += val/float(len(vec_a))
		
	mean_y = np.zeros(n_loc)
	for val in vec_b:
		mean_y += val/float(len(vec_b))
		
	#print mean_x
	#print mean_y
	
	return mean_x, mean_y
		

			


if __name__ == "__main__":
	
	if len(sys.argv) != 4:
		sys.exit("Missuse detected")

	folder = sys.argv[1]
	outfile = sys.argv[2]
	n = int(sys.argv[3])
	
	fig = figure(figsize=(6,6))
	ax = fig.add_subplot(1,1,1,aspect='equal')
	
	
	x,y = getTraj("MovieB/MovieB_peaks_I.dat", n)
	#ax.plot(x, y, '-8', c='gray', label="Region SW")
	ax.plot(x, y, '-o', label="Region SW")
	
	x,y = getTraj("MovieB/MovieB_peaks_II.dat", n)
	#ax.plot(x, y, '-^', c='gray', label="Region SE")
	ax.plot(x, y, '-o', label="Region SE")
	
	x,y = getTraj("MovieB/MovieB_peaks_III.dat", n)
	#ax.plot(x, y, '-v', c='gray', label="Region NW")
	ax.plot(x, y, '-o', label="Region NW")
	
	x,y = getTraj("MovieB/MovieB_peaks_IV.dat", n)
	#plt.plot(x, y, '->', c='gray', label="Region NE")
	plt.plot(x, y, '-o', label="Region NE")
	
	plt.plot([0], [0], 'sk')
	
	#plt.xlabel(r'Drift X [$\AA$]')
	#plt.ylabel(r'Drift Y [$\AA$]')
	plt.xlabel('Drift X [px]')
	plt.ylabel('Drift Y [px]')
	
	plt.grid()
	
	ax.axis('equal')
	ax.legend(loc=1)
	
	plt.savefig(outfile)
	
