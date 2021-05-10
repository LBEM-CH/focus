'''
A python file to plot the FSC from it's text file

Author: Nikhil Biyani (nikhilbiyani@gmail.com)

Usage:
	python plotFCC.py <Data file with cone angle in col1 and FCC in col 2> <Output plot file>

Example:
	python plotFCC.py fcc.dat fcc.ps
'''

import sys
try:
    import numpy
    import matplotlib.pyplot as plt
except ImportError:
    raise ImportError("Import Error: numpy/matplotlib was not found!!\nHint: Try installing EMAN2.")

#-------------------------------
# Check for arguments
#------------------------------
if len(sys.argv) < 3:
    print ( "Usage:\n\tpython " + sys.argv[0] + " <FCC data file> <Output plot file>\n\n" )
    sys.exit(1)

fcc_file = sys.argv[1]
output_file = sys.argv[2]

#--------------------------------
# Read data from file
#--------------------------------
data = numpy.loadtxt(fcc_file, skiprows=3)
data = data.transpose();

#--------------------------------
# Plot the data
#--------------------------------

# Setup data
plt.plot(data[0], data[1], 'b-')
plt.xlim(0, max(data[0]))
plt.ylim(0, 1.0)

# Setup title
plt.title("FCC of the data")
plt.xlabel('Cone angle (in degrees)')
plt.ylabel('Fourier conic correlation')


# Custom  x-axis labels
ticks = numpy.linspace(min(data[0]), max(data[0]), 10)
labels = numpy.round((ticks), 1)
plt.xticks(ticks.tolist(), labels.tolist())

# Save plot to file
plt.savefig(output_file)
