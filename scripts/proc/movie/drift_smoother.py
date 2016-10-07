from pylab import *
import os
from numpy import array

class peak:
	def __init__(self, pos_x, pos_y, start_x, start_y, end_x, end_y, peak, h, k, rocenx, roceny):
		self.pos_x = pos_x
		self.pos_y = pos_y
		self.start_x = start_x
		self.start_y = start_y
		self.end_x = end_x
		self.end_y = end_y
		self.peak = peak
		self.h = h
		self.k = k
		self.rocenx = rocenx
		self.roceny = roceny
		
def radial_sort(peak_list, peak):
	x = peak.pos_x
	y = peak.pos_y
	return sorted(peak_list, key=lambda peak: sqrt((peak.pos_x-x)**2 + (peak.pos_y-y)**2) )

if __name__ == "__main__":
	
	if len(sys.argv) != 6:
		sys.exit("Usage: drift_smoother.py <Data-File-IN> <Data-File-OUT> <N> <PROFDATA> <IFRAMS>")

	infile = sys.argv[1]
	outfile = sys.argv[2]
	N = int(sys.argv[3])
	oldprof_file = sys.argv[4]
	IFRAMS = int(sys.argv[5])

	print "IFRAMS = ",IFRAMS
	
	data_file = open(infile)
	data_file_out = open(outfile,'w')
	
	# move header 
	for i in range(0,13):
		data_file_out.write(data_file.readline())
		
	peaks=[]
	
	for l in data_file:
		data_split = l.split()
		
		p = peak(float(data_split[2]), float(data_split[3]), float(data_split[5]), 
		float(data_split[6]), float(data_split[7]), float(data_split[8]), 
		float(data_split[4]), float(data_split[0]), float(data_split[1]), 
		float(data_split[9]), float(data_split[10]))
		
		peaks.append(p)
	
	for p in peaks:
		peaks_sorted = radial_sort(peaks,p)
		x_lens = []
		y_lens = []
		for i in range(0,N):
			x_lens.append(peaks_sorted[i].start_x)
			y_lens.append(peaks_sorted[i].start_y)
		p.mean_start_x = np.mean(x_lens)
		p.mean_start_y = np.mean(y_lens)
		del x_lens[:]
		del y_lens[:]

		x_lens = []
		y_lens = []
		for i in range(0,N):
			x_lens.append(peaks_sorted[i].end_x)
			y_lens.append(peaks_sorted[i].end_y)
		p.mean_end_x = np.mean(x_lens)
		p.mean_end_y = np.mean(y_lens)
		del x_lens[:]
		del y_lens[:]

		x_lens = []
		y_lens = []
		for i in range(0,N):
			x_lens.append(peaks_sorted[i].rocenx)
			y_lens.append(peaks_sorted[i].roceny)
		p.mean_rocenx = np.mean(x_lens)
		p.mean_roceny = np.mean(y_lens)
		del x_lens[:]
		del y_lens[:]

		
	for p in peaks:
		line = str(int(p.h)) + "\t"
		line += str(int(p.k)) + "\t"
		line += str(p.pos_x) + "\t"
		line += str(p.pos_y) + "\t"
		line += str(p.peak) + "\t"
		line += str(p.mean_start_x) + "\t"
		line += str(p.mean_start_y) + "\t"
		line += str(p.mean_end_x) + "\t"
		line += str(p.mean_end_y) + "\t"
		line += str(p.mean_rocenx) + "\t"
		line += str(p.mean_roceny) + "\n"
		data_file_out.write(line)
	
	data_file.close()
	data_file_out.close()

	oldPROF = open(oldprof_file)
	oldlines = oldPROF.readlines()
	oldPROF.close()

	for iframe in range(1,IFRAMS+1):
		proffile = 'frames/PROFDATA_' + str(int(iframe)) + '.dat'
		print "Opening ", proffile
		data_file_out = open(proffile,'w')
		for line in oldlines[0:7]:
			data_file_out.write(line)
		for p in peaks:
			rx = p.pos_x + p.mean_start_x*(1.0*IFRAMS-iframe)/(IFRAMS-1.0) + p.mean_end_x*(1.0*iframe-1)/(IFRAMS-1.0) + p.mean_rocenx
			ry = p.pos_y + p.mean_start_y*(1.0*IFRAMS-iframe)/(IFRAMS-1.0) + p.mean_end_y*(1.0*iframe-1)/(IFRAMS-1.0) + p.mean_roceny
			line = str(int(p.h)) + "\t"
			line += str(int(p.k)) + "\t"
			line += str(rx) + "\t"
			line += str(ry) + "\t"
			line += str(p.peak) + "\n"
			data_file_out.write(line)
		data_file_out.close()				








