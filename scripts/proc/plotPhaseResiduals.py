'''
A python file to plot the phase residuals from it's text file

Author: Nikhil Biyani (nikhilbiyani@gmail.com)

Usage:
	python plotPhaseResiduals.py <Data file with freq in col1 and residuals in col 2> <Output plot file>

Example:
	python plotPhaseResiduals.py residuals.dat residuals.ps
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
    print ( "Usage:\n\tpython " + sys.argv[0] + " <Residuals file> <Output plot file>\n\n" )
    sys.exit(1)

residuals_file = sys.argv[1]
output_file = sys.argv[2]

#--------------------------------
# Read data from file
#--------------------------------
data = numpy.loadtxt(residuals_file, skiprows=3)
data = data.transpose();

#--------------------------------
# Plot the data
#--------------------------------

# Setup data
plt.plot(data[0], data[1], 'b-')
plt.xlim(0, max(data[0]))
plt.ylim(0, 100)

# Setup title
plt.title("Phase residuals of merged data")
plt.xlabel('resolution (' + r'$\AA$' + ')')
plt.ylabel('Phase residuals (degrees)')


# Custom  x-axis labels
ticks = numpy.linspace(min(data[0]), max(data[0]), 10)
labels = numpy.round(numpy.reciprocal(ticks), 1)
plt.xticks(ticks.tolist(), labels.tolist())

# Add 60 degree line
plt.axhline(y=60, xmin=0, xmax=1, hold=None, color='r', label='Phase residual of 60 degrees')
plt.legend(loc=4)

# Save plot to file
plt.savefig(output_file)
