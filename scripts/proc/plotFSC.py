'''
A python file to plot the FSC from it's text file

Author: Nikhil Biyani (nikhilbiyani@gmail.com)

Usage:
	python plotFSC.py <Data file with freq in col1 and fsc in col 2> <Output plot file>

Example:
	python plotFSC.py fsc.dat fsc.ps
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
    print "Usage:\n\tpython " + sys.argv[0] + " <FSC file> <Output plot file>\n\n"
    sys.exit(1)

fsc_file = sys.argv[1]
output_file = sys.argv[2]

#--------------------------------
# Read data from file
#--------------------------------
data = numpy.loadtxt(fsc_file, skiprows=3)
data = data.transpose();

#--------------------------------
# Plot the data
#--------------------------------

# Setup data
plt.plot(data[0], data[1], 'b-')
plt.xlim(0, max(data[0]))
plt.ylim(0, 1.0)

# Setup title
plt.title("FSC of merged data")
plt.xlabel('resolution (' + r'$\AA$' + ')')
plt.ylabel('Fourier shell correlation')


# Custom  x-axis labels
ticks = numpy.linspace(min(data[0]), max(data[0]), 10)
labels = numpy.round(numpy.reciprocal(ticks), 1)
plt.xticks(ticks.tolist(), labels.tolist())

# Add 0.143 and 0.5 degree line
plt.axhline(y=0.143, xmin=0, xmax=1, hold=None, color='r', label='FSC of 0.143')
plt.axhline(y=0.5, xmin=0, xmax=1, hold=None, color='g', label='FSC of 0.5')
plt.legend()

# Save plot to file
plt.savefig(output_file)