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
		
		yy = []
		for class_data in split_line[1:]:
			yy.append(float(class_data))
		y.append(yy)
		
	return [x,y]
	
def generatePerClassNumbers(data):
	result = []
	
	for i in range(0,len(data[0])):
		result.append([])
		
	for i in range(0,len(data)):
		for j in range(0,len(data[0])):
			result[j].append(data[i][j])
			
	return result

	
if __name__ == '__main__':


	if len(sys.argv) != 3:
		sys.exit("Usage: python mra_plotter.py [filename] [filename out]")

	data = readFile(sys.argv[1])
	x = range(1,len(data[0])+1)
	
	figure()
	
	ax = subplot(1,1,1)
	ax.xaxis.set_major_locator(MaxNLocator(integer=True))
	
	ax.plot(x, data[0], "k--")
	ax.set_ylabel('number of particles changing the class')
	
	class_data = generatePerClassNumbers(data[1])
		
	ax2 = ax.twinx()
	
	for data in class_data:
		ax2.plot(x, data)
	ax2.set_ylabel('classes')
	
	plt.title('Convergence of the MRA-Classification')
	plt.xlabel('n Iteration')
	
	plt.savefig(str(sys.argv[2]))
