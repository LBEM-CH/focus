import sys
from pylab import *
from matplotlib import rc


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
		sys.exit("Usage: python fsc_potter.py [filename] [outfile]")

	data = readFile(sys.argv[1])
	
	radii = copy(data[0])

	for i in range(0,len(data[0])):
		data[0][i] = data[0][i] / (2*max(data[0]))
	
	apix = 1.0
	for i in range(0,len(data[0])):
		if data[0][i] > 0.01:
			print radii[i], apix/data[0][i]
		if ( data[1][i] < 0.5 ):
			res_freq = data[0][i]
			break
	
	res = apix / res_freq
	
	n = int(len(data[0])*0.4)
	
	print round(res,3)
	
	#fill_between(data[0], list(repeat(0, len(data[0]))), data[1], facecolor='green', interpolate=False)
	plot(data[0][0:n], data[1][0:n], color = 'k', linewidth=2)
	plot([0,0.20], [0.5,0.5], '--k', linewidth=1)
	#title('Fourier Shell Correlation (Resolution=' + str(round(res,3)) +'A)')
	plt.ylabel('FSC')
	plt.xlabel('Resolution (1/A)')
	plt.savefig(sys.argv[2])
	
