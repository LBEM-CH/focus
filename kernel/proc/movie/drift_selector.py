from pylab import *
import os
from numpy import array
from EMAN2 import *
from sparx import *

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
	
	print "drift_selector.py"

	if len(sys.argv) != 13:
		sys.exit("Usage: drift_selector.py <Data-File-IN> <Data-File-OUT> <drift_tolerance> <PROFDATA> <number_of_frames> <imagesidelength> <original_mask> <new_mask> <4 x lattice>")

	infile = sys.argv[1]
	outfile = sys.argv[2]
	diff_threshold = float(sys.argv[3])
	oldprof_file = sys.argv[4]
	IFRAMS = int(sys.argv[5])
	imagesidelength = int(sys.argv[6]) 
	mask_infile = sys.argv[7]
	mask_outfile = sys.argv[8]
	lat_u1 = float(sys.argv[9])
	lat_u2 = float(sys.argv[10])
	lat_v1 = float(sys.argv[11])
	lat_v2 = float(sys.argv[12])

	N = 4

	print "IFRAMS = ",IFRAMS

	data_file = open(infile)
	mask_image = get_image(mask_infile)
	s = info(mask_image)
	masksidelength = s[4]
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
		for i in range(1,N):
			x_lens.append(peaks_sorted[i].end_x)
			y_lens.append(peaks_sorted[i].end_y)
		diff = sqrt((p.end_x - np.mean(x_lens))**2 + (p.end_y - np.mean(y_lens))**2)
		if diff > diff_threshold:
			p.peak = 0.0
		del x_lens[:]
		del y_lens[:]

		x_lens = []
		y_lens = []
		for i in range(1,N):
			x_lens.append(peaks_sorted[i].rocenx)
			y_lens.append(peaks_sorted[i].roceny)
		diff = sqrt((p.rocenx - np.mean(x_lens))**2 + (p.roceny - np.mean(y_lens))**2)
		if diff > diff_threshold:
			p.peak = 0.0
		del x_lens[:]
		del y_lens[:]

		
	for p in peaks:
		if p.peak != 0.0:
			line = str(int(p.h)) + "\t"
			line += str(int(p.k)) + "\t"
			line += str(p.pos_x) + "\t"
			line += str(p.pos_y) + "\t"
			line += str(p.peak) + "\t"
			line += str(p.start_x) + "\t"
			line += str(p.start_y) + "\t"
			line += str(p.end_x) + "\t"
			line += str(p.end_y) + "\t"
			line += str(p.rocenx) + "\t"
			line += str(p.roceny) + "\n"
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
			if p.peak != 0.0:
				rx = p.pos_x + p.start_x*(1.0*IFRAMS-iframe)/(IFRAMS-1.0) + p.end_x*(1.0*iframe-1)/(IFRAMS-1.0) + p.rocenx
				ry = p.pos_y + p.start_y*(1.0*IFRAMS-iframe)/(IFRAMS-1.0) + p.end_y*(1.0*iframe-1)/(IFRAMS-1.0) + p.roceny
				line = str(int(p.h)) + "\t"
				line += str(int(p.k)) + "\t"
				line += str(rx) + "\t"
				line += str(ry) + "\t"
				line += str(p.peak) + "\n"
				data_file_out.write(line)
		data_file_out.close()				


	image = model_blank(imagesidelength,imagesidelength,1,0.0)
	len_u = sqrt(lat_u1 ** 2 + lat_u2 ** 2)
	len_v = sqrt(lat_v1 ** 2 + lat_v2 ** 2)
	# the disk diameter should be twice as long as the lattice vectors: 
	disk_radius = int(max(len_u,len_v))
	sigma = 0.15/disk_radius
	disk = model_circle(120.0, imagesidelength, imagesidelength,1)
	for p in peaks:
		if p.peak != 0.0:
			image	+= cyclic_shift(disk,p.pos_x+p.rocenx+(imagesidelength/2)+1,p.pos_y+p.roceny+(imagesidelength/2)+1)
	image2 = threshold_maxval(image,1.0)
	print "filtering mask image with Gaussian low-pass filter, using sigma = ", sigma
	image = filt_gaussl(image2,sigma)
	ratio = 1.0 * masksidelength / imagesidelength
	print "Upscaling by a factor of %10.6f" % ratio
	image2 = resample(image,ratio)
	s = info(image2)
	newsize = s[4]
	print "image2 size = ", newsize
	s = info(mask_image)
	newsize = s[4]
	print "mask size = ", newsize
	mask_image *= image2
	mask_image.write_image(mask_outfile)
	print "written mask_outfile as ",mask_outfile



