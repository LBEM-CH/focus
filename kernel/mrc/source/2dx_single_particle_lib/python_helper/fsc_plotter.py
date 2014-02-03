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
		sys.exit("Usage: python fsc_potter.py [filename] [file_out]")

	data = readFile(sys.argv[1])

	for i in range(0,len(data[0])):
		data[0][i] = data[0][i] / (2*max(data[0]))
	
	for i in range(0,len(data[0])):
		if ( data[1][i] < 0.5 ):
			res_freq = data[0][i]
			break
	
	apix = 1
	res = apix / res_freq
	
	fill_between(data[0], list(repeat(0, len(data[0]))), data[1], facecolor='green', interpolate=False)
	plot(data[0], data[1], color = 'k', linewidth=1)
	plot([0,0.5], [0.5,0.5], '--k', linewidth=2)
	title('Fourier Shell Correlation (Resolution=' + str(round(res,3)) +'A)')
	plt.ylabel('FSC(r)')
	plt.xlabel('freq')
	plt.savefig(sys.argv[2])
	
