#!/usr/bin/python

from EMAN2  import *
from sparx  import *

from pylab import plt, plot, subplot, figure, hist

import sys

filename = sys.argv[1]
outfolder = sys.argv[2]

from MotionCorrectionWatcher import *

watcher = MotionCorrectionWatcher(1,2,3,4,5)

print filename
print watcher.getFileCoreName(filename)
print outfolder
log_motion_file = outfolder + "/dosef_quick" + watcher.getFileCoreName(filename) + "_ready_Log.txt"
print log_motion_file
content = read_text_row(log_motion_file)

x = []
y = []
for c in content:
	if c != []:
		if c[0] == "......Shift":
			x.append(float(c[5]))
			y.append(float(c[6]))
		
plot(x,y, 'o-')
plt.title('Shift profile')
plt.xlabel('shift x (pixel)')
plt.ylabel('shift y (pixel)')
plt.savefig(outfolder + "/dosef_quick/" + watcher.getFileCoreName(filename) + ".tif")
plt.close()

sx_power = get_image(outfolder + "/dosef_quick/" + watcher.getFileCoreName(filename) + "_ready_CorrFFT.mrc")
sx_rot = rot_avg(sx_power)
r_fft = sx_rot.get_data_as_vector()
max_index = int(0.8 * len(r_fft) )
plot(r_fft[:max_index], linewidth=2.0, label="drift corrected")

sx_power = get_image(outfolder + "/dosef_quick/" + watcher.getFileCoreName(filename) + "_ready_RawFFT.mrc")
sx_rot = rot_avg(sx_power)
r_fft = sx_rot.get_data_as_vector()
plot(r_fft[:max_index], label="original")
		
plt.legend()
plt.savefig(outfolder + "/dosef_quick/" + watcher.getFileCoreName(filename) + "_rotpower.tif")
plt.close()
