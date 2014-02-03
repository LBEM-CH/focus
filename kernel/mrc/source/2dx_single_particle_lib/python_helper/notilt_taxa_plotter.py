import sys
from pylab import *
from matplotlib import rc


def readFile(filename):
	x = []
	infile = open( filename, "r" )

	for line in infile:
		split_line = line.split()
		x.append(float(split_line[0]))
	
	return [x]


if __name__ == '__main__':
	
	if len(sys.argv) != 2:
		sys.exit("Usage: python notilt_taxa_plotter.py [filename]")

	data = readFile(sys.argv[1])
	bins = list(range(0,90))
	hist(data, bins, normed=True)
	plt.savefig('inplace_taxa_dist.pdf')
	
