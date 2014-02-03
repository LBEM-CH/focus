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
		sys.exit("Usage: python convergence_plotter.py [filename] [filename out]")

	data = readFile(sys.argv[1])
	
	figure()
	
	ax = subplot(1,1,1)
	ax.xaxis.set_major_locator(MaxNLocator(integer=True))
	
	ax.plot(data[0], data[1], "o-")
	plt.title('Convergence of the refinement')
	
	
	plt.xlabel('n Iteration')
	plt.ylabel('Angular Change')
	plt.savefig(str(sys.argv[2]))

