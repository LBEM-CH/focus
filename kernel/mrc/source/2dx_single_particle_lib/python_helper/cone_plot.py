import sys
from pylab import *
from matplotlib import rc
import numpy as np
from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as plt



def readFile(filename):
	x = []
	y = []
	infile = open( filename, "r" )

	for line in infile:
		split_line = line.split()
		x.append(float(split_line[0]))
		y.append(float(split_line[1]))
	
	return [x,y]


if __name__ == '__main__':
	
	if len(sys.argv) != 2:
		sys.exit("Usage: python cone_plotter.py [filename]")
		
	data = readFile(sys.argv[1])
	
	fig = plt.figure()
	
	hist(data[0], bins=1000)	
	plt.savefig('test_hist.pdf')
	
	hist(data[1], bins=1000)	
	plt.savefig('test_hist2.pdf')
	
