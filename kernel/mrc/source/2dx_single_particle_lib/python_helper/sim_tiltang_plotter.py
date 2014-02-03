import sys
from pylab import *
from matplotlib import rc
from numpy import arange,array,ones,linalg



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


	if len(sys.argv) != 3:
		sys.exit("Usage: python sim_tiltang_plotter.py [filename] [filename out]")

	data = readFile(sys.argv[1])
	
	figure()
	plt.plot(data[0], data[1], "o")
	
	A = array([ data[0], ones(len(data[0]))])
	w = linalg.lstsq(A.T,data[1])[0]
	line = []
	for x in data[0]:
		line.append(w[0]*x+w[1])
	plot(data[0], line,'r-')
	
	plt.title('TiltAng vs CC-Value')
	plt.xlabel('TiltAng')
	plt.ylabel('CC-Value')
	plt.savefig(str(sys.argv[2]))
