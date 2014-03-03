#!/usr/bin/python

from EMAN2  import *
from sparx  import *

from pylab import plt, plot, subplot, figure, hist

import sys

def convertToFloat(data_in):
	data_out = []
	for d in data_in:
		data_out.append(float(d))
	return data_out




output_dir = sys.argv[1]
data_file_name = output_dir + "/automatic/stat_data.txt"

content = os.listdir (output_dir + "/automatic/")
for c in content:
	if c.endswith(".tif"):
		os.remove(output_dir + "/automatic/" + c)

data_file = open(data_file_name, 'r')

data = data_file.readline()
data = data.split()
tilts = convertToFloat(data)

data = data_file.readline()
data = data.split()
defs = convertToFloat(data)

data = data_file.readline()
data = data.split() 
qvals = convertToFloat(data)


ang_min = 0
ang_max = max(tilts)
ang_dx = 2
			
hist(tilts, bins=(ang_max-ang_min)/ang_dx, range=(ang_min,ang_max), facecolor='blue', alpha=0.5)
plt.title('Tilt Angle Histogram')
plt.xlabel('Tilt Angle (degrees)')
plt.ylabel('Counts')
plt.savefig(output_dir + "/automatic/tilts.tif")
plt.close()
		
def_min = 0
def_max = max(defs)
def_dx = 1000
				
defs_pertilt = [[],[],[],[]]
angs_pertilt = [[],[],[],[]]
qvals_pertilt = [[],[],[],[]]
		
for i in range(len(defs)):
	if abs(tilts[i]) < 10:
		angs_pertilt[0].append(tilts[i])
		defs_pertilt[0].append(defs[i])
		qvals_pertilt[0].append(qvals[i])
	elif abs(tilts[i]) < 20 and abs(tilts[i])>=10:
		angs_pertilt[1].append(tilts[i])
		defs_pertilt[1].append(defs[i])
		qvals_pertilt[1].append(qvals[i])
	elif abs(tilts[i]) < 30 and abs(tilts[i])>=20:
		angs_pertilt[2].append(tilts[i])
		defs_pertilt[2].append(defs[i])
		qvals_pertilt[2].append(qvals[i])
	else:
		angs_pertilt[3].append(tilts[i])
		defs_pertilt[3].append(defs[i])
		qvals_pertilt[3].append(qvals[i])
		
hist(defs, bins=(def_max-def_min)/def_dx, range=(def_min,def_max), facecolor='green', alpha=0.5)
plt.title('Defoci Histogram')
plt.xlabel('Defocus (A)')
plt.ylabel('Counts')
plt.savefig(output_dir + "/automatic/defoci.tif")
plt.close()
		
qval_min = min(qvals)
qval_max = max(qvals)
qval_n_bins = 40
		
hist(qvals, bins=qval_n_bins, range=(qval_min,qval_max), facecolor='red', alpha=0.5)
plt.title('QVal Histogram')
plt.xlabel('QVal')
plt.ylabel('Counts')
plt.savefig(output_dir + "/automatic/qval.tif")
plt.close()
		
plt.subplots_adjust(hspace=0.6)
if len(qvals_pertilt[0])>0:
	plt.subplot(411)
	hist(qvals_pertilt[0], bins=qval_n_bins, range=(qval_min,qval_max), facecolor='red', alpha=0.5)
	plt.title('QVAL Histogram (0-10 degrees)')
	plt.ylabel('Counts')
	if len(qvals_pertilt[1])==0 and len(qvals_pertilt[2])==0 and len(qvals_pertilt[3])==0:
		plt.xlabel('QVAL')
if len(qvals_pertilt[1])>0:
	plt.subplot(412)
	hist(qvals_pertilt[1], bins=qval_n_bins, range=(qval_min,qval_max), facecolor='red', alpha=0.5)
	plt.title('QVAL Histogram (10-20 degrees)')
	plt.ylabel('Counts')
	if len(qvals_pertilt[2])==0 and len(qvals_pertilt[3])==0:
		plt.xlabel('QVAL')
if len(qvals_pertilt[2])>0:
	plt.subplot(413)
	hist(qvals_pertilt[2], bins=qval_n_bins, range=(qval_min,qval_max), facecolor='red', alpha=0.5)
	plt.title('QVAL Histogram (20-30 degrees)')
	plt.ylabel('Counts')
	if len(qvals_pertilt[3])==0:
		plt.xlabel('QVAL')
if len(qvals_pertilt[3])>0:
	plt.subplot(414)
	hist(qvals_pertilt[3], bins=qval_n_bins, range=(qval_min,qval_max), facecolor='red', alpha=0.5)
	plt.title('QVAL Histogram (>30 degrees)')
	plt.ylabel('Counts')
	plt.xlabel('QVAL')
		
plt.savefig(output_dir + "/automatic/qvals.tif")
plt.close()
		
plt.subplot(111)
plt.plot(defs, qvals, 'o', alpha=0.6)
plt.title('Defocus vs. QVAL')
plt.ylabel('QVAL')
plt.xlabel('Defocus')
plt.savefig(output_dir + "/automatic/tilt_qval.tif")
plt.close()
		
plt.subplots_adjust(hspace=0.6)
if len(defs_pertilt[0])>0:
	plt.subplot(411)
	hist(defs_pertilt[0], bins=(def_max-def_min)/def_dx, range=(def_min,def_max), facecolor='green', alpha=0.5)
	plt.title('Defocus Histogram (0-10 degrees)')
	plt.ylabel('Counts')
	if len(defs_pertilt[1])==0 and len(defs_pertilt[2])==0 and len(defs_pertilt[3])==0:
		plt.xlabel('Defocus (A)')
if len(defs_pertilt[1])>0:
	plt.subplot(412)
	hist(defs_pertilt[1], bins=(def_max-def_min)/def_dx, range=(def_min,def_max), facecolor='green', alpha=0.5)
	plt.title('Defocus Histogram (10-20 degrees)')
	plt.ylabel('Counts')
	if len(defs_pertilt[2])==0 and len(defs_pertilt[3])==0:
		plt.xlabel('Defocus (A)')
if len(defs_pertilt[2])>0:
	plt.subplot(413)
	hist(defs_pertilt[2], bins=(def_max-def_min)/def_dx, range=(def_min,def_max), facecolor='green', alpha=0.5)
	plt.title('Defocus Histogram (20-30 degrees)')
	plt.ylabel('Counts')
	if len(defs_pertilt[3])==0:
		plt.xlabel('Defocus (A)')
if len(defs_pertilt[3])>0:
	plt.subplot(414)
	hist(defs_pertilt[3], bins=(def_max-def_min)/def_dx, range=(def_min,def_max), facecolor='green', alpha=0.5)
	plt.title('Defocus Histogram (>30 degrees)')
	plt.ylabel('Counts')
	plt.xlabel('Defocus (A)')
		
plt.savefig(output_dir + "/automatic/defocis.tif")
plt.close()
