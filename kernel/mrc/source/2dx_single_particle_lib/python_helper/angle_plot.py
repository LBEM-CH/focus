import sys
from pylab import *
from matplotlib import rc
from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as plt



def readFile(filename):
	x = []
	y = []
	z = []
	infile = open( filename, "r" )

	for line in infile:
		split_line = line.split()
		x.append(float(split_line[0]))
		y.append(float(split_line[1]))
		z.append(float(split_line[2]))
		
	return [x,y,z]


if __name__ == '__main__':
	
	if len(sys.argv) != 2:
		sys.exit("Usage: python anlge_plotter.py [filename]")
		
	X = np.arange(-1, 1.2, 0.25)
	xlen = len(X)
	Y = np.arange(-1, 1.2, 0.25)
	ylen = len(Y)
	X, Y = np.meshgrid(X, Y)
	R = np.sqrt(np.zeros((xlen,ylen)))
	Z = np.sin(R)

	data = readFile(sys.argv[1])
	
	fig = plt.figure()
	ax = fig.add_subplot(111, projection='3d')

	n = 400

	#ax.plot_wireframe(data[0][:n], data[1][:n], data[2][:n])


	ax.scatter(data[0][n:], data[1][n:], data[2][n:], c="r", marker="^")

	
	#for i in range(1,len(data[0])-2):
	#	ax.plot((0,data[0][i]), (0,data[1][i]), (0,data[2][i]), c="y")
	
	


	
	#surf = ax.plot_surface(X, Y, Z, rstride=1, cstride=1,linewidth=0, antialiased=False)

	ax.set_zlim3d(-1, 1)

	
	
	plt.show()
	
	
	#plt.savefig('FSC.pdf')
	
