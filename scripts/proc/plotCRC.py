'''
A python file to plot the FSC from it's text file

Author: Nikhil Biyani (nikhilbiyani@gmail.com)

Usage:
	python plotFCMC.py <Data file with freq in col1 cone in col2 and correlation in col3> <Output plot file>

Example:
	python plotFCMC.py fsmc.dat fsmc.ps
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
    print "Usage:\n\tpython " + sys.argv[0] + " <FSMC data file> <Output plot file>\n\n"
    sys.exit(1)

fcmc_file = sys.argv[1]
output_file = sys.argv[2]

#--------------------------------
# Read data from file
#--------------------------------
resolutions = numpy.genfromtxt(fcmc_file, skiprows=3, dtype=float, usecols=(0))
heights = numpy.genfromtxt(fcmc_file, skiprows=3, dtype=float, usecols=(1))
correlations = numpy.genfromtxt(fcmc_file, skiprows=3, dtype=float, usecols=(2))

# Mirror the data along X-axis
res_gt_0 = resolutions != 0.0
resolutions = numpy.concatenate((resolutions, -1*resolutions[res_gt_0]), axis=1)
heights = numpy.concatenate((heights, heights[res_gt_0]), axis=1)
correlations = numpy.concatenate((correlations, correlations[res_gt_0]), axis=1)

hts_gt_0 = heights != 0.0
resolutions = numpy.concatenate((resolutions, resolutions[hts_gt_0]), axis=1)
heights = numpy.concatenate((heights, -1*heights[hts_gt_0]), axis=1)
correlations = numpy.concatenate((correlations, correlations[hts_gt_0]), axis=1)

#--------------------------------
# Plot the data
#--------------------------------

res_points = numpy.unique(resolutions)
height_points = numpy.unique(heights)

res_mesh = resolutions.reshape(len(res_points), len(height_points));
height_mesh = heights.reshape(len(res_points), len(height_points));
corr_mesh = correlations.reshape(len(res_points), len(height_points));

corr_mesh[corr_mesh < 0.0] = 0.0 

plt.scatter(resolutions, heights, c=correlations, cmap='GnBu', alpha=0.9, linewidth=0.0)
#plt.pcolormesh(res_mesh, height_mesh, corr_mesh, cmap='GnBu')
plt.colorbar();
plt.xlim(min(res_points), max(res_points))
plt.ylim(min(height_points), max(height_points))
plt.xlabel('XY-Resolution (' + r'$\AA$' + ')')
plt.ylabel('Height (' + r'$\AA$' + ')')


# Custom  x/y-axis labels
ticksx = numpy.linspace(min(resolutions), max(resolutions), 10)
labelsx = numpy.round(numpy.reciprocal(ticksx), 1)
plt.xticks(ticksx.tolist(), labelsx.tolist())

# Custom  x/y-axis labels
ticksy = numpy.linspace(min(heights), max(heights), 10)
labelsy = numpy.round(numpy.reciprocal(ticksy), 1)
plt.yticks(ticksy.tolist(), labelsy.tolist())

# Save plot to file
plt.savefig(output_file)