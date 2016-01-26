'''
A python file to extract beam tilt values from 
the refined tile console CSV file.

Author: Nikhil Biyani (nikhilbiyani@gmail.com)

Usage:
	python plotTileBeamTilt.py <CSV file> <number of tiles along one direction> <Output Image file>

Example:
	python plotTileBeamTilt.py 2dx_mergeRefine.console.csv 5 tileBeamTilt.png
'''

import sys
import csv
try:
	import numpy as np
	import matplotlib.pyplot as plt
	import matplotlib.gridspec as gridspec
except ImportError:
        raise ImportError("Import Error: numpy/matplotlib was not found!!\nHint: Try installing EMAN2.")

#-------------------------------
# Check for arguments
#------------------------------
if len(sys.argv) < 4:
	print "Usage:\n\tpython " + sys.argv[0] + " <CSV file> <number of tiles along one direction> <Output Image file>\n\n"
	sys.exit(1)

csv_file = sys.argv[1]
number_of_tiles = int(sys.argv[2])
output_file = sys.argv[3]

#--------------------------------
# Read beamtilts from CSV File
#--------------------------------
beamtiltsx = np.zeros(25, dtype=np.float)
beamtiltsy = np.zeros(25, dtype=np.float)
count = 0
countsx = np.zeros(25, dtype=np.int)
countsy = np.zeros(25, dtype=np.int) 
bt_startx = 3 + 2*number_of_tiles*number_of_tiles
bt_starty = bt_startx + number_of_tiles*number_of_tiles
try:
	with open(csv_file, 'rb') as f:
		reader = csv.reader(f, delimiter=',')
    		for row in reader:
			count += 1
        		for i in range(number_of_tiles*number_of_tiles):
				if not row[bt_startx+i] == "":
					beamtiltsx[i] += float(row[bt_startx+i])
					countsx[i] += 1
				if not row[bt_starty+i] == "":
					beamtiltsy[i] += float(row[bt_starty+i])
					countsy[i] += 1
except Exception,e:
	print "Exception occured while reading the file:"
	print str(e)
	exit(1)

beamtiltsx = np.divide(beamtiltsx, countsx)
beamtiltsy = np.divide(beamtiltsy, countsy)

print "\nFinished reading " + str(count) + " entries in the file.\n"
print "\nFollwing were the beamtilts and counts in x:"
print beamtiltsx
print countsx
print "\nFollwing were the beamtilts and counts in y:"
print beamtiltsy
print countsy


#---------------------------------
# Make a plot
#--------------------------------

range_plot = max(abs(min(beamtiltsx)), abs(max(beamtiltsx)), abs(min(beamtiltsy)), abs(max(beamtiltsy)))
range_plot = range_plot+0.4*range_plot

fig = plt.figure(figsize = (number_of_tiles, number_of_tiles))
fig.suptitle("Averaged beamtilts from " + str(count) + " measurements\nTile axes range: (-" + str(round(range_plot,4)) + ", " +str(round(range_plot,4)) + ") mrad.", fontsize=10)

gs = gridspec.GridSpec( number_of_tiles, number_of_tiles)
gs.update(wspace=0.00, hspace=0.00) # set the spacing between axes. 

for i in range(0, number_of_tiles):
        for j in range(0, number_of_tiles):
                print "Plotting for tile: " + str(i+1) + " " + str(j+1)
                ax = plt.subplot(gs[j*number_of_tiles +i])
                ax.arrow(0, 0, beamtiltsx[j*number_of_tiles +i], beamtiltsy[j*number_of_tiles +i], head_width=0.2*range_plot, head_length=0.2*range_plot, linewidth=2.0*range_plot, fc='r', ec='r', alpha=0.7)
                ax.set_xlim(-1*range_plot, range_plot)
                ax.set_ylim(-1*range_plot, range_plot)
                ax.set_axis_off()
                if j == 0:
                    ax.axhline(y=range_plot, color='k')
                if i == 0:
                    ax.axvline(x=-1*range_plot, color='k')
                if not j == number_of_tiles-1:
                    ax.axhline(y=-1*range_plot, linestyle='dashed', alpha=0.5, color='k')
                else:
                    ax.axhline(y=-1*range_plot, color='k')
                if not i == number_of_tiles-1:
                    ax.axvline(x=range_plot, linestyle='dashed', alpha = 0.5, color='k')
                else:
                    ax.axvline(x=range_plot, color='k')

plt.savefig(output_file)
