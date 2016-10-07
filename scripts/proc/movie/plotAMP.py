import matplotlib.pyplot as plt
from matplotlib.pyplot import figure, show
import numpy as np

import math

import os
import sys

def getFloatArray(array_str):
	result = []
	for item in array_str:
		result.append(float(item))
	return result

def getResBin(res):
	if res>=4 and res<5:
		return 0
	elif res>=5 and res<7:
		return 1
	elif res>=7 and res<10:
		return 2
	elif res>=10 and res<20:
		return 3
	elif res>=20 and res<80:
		return 4
	else:
		return -1
		
def getAverages(data):
	aves = []
	for r in range(0,len(data)):
		a = np.array(data[r])
		print np.mean(a, axis=0)
		aves.append(np.mean(a, axis=0))
	return aves
	

if __name__ == "__main__":
	
	if len(sys.argv) != 4:
		sys.exit("Missuseage detected, usage exe mode stating ending")

	amp_file = sys.argv[1]
	amp_pdf_file = sys.argv[2]
	batch_size = int(sys.argv[3])
	
	print "plotting amps for", amp_file, "with batch size", batch_size
	
	data_file = open(amp_file)
	
	res_data = []
	for i in range(0,5):
			res_data.append([])
	
	for l in data_file:
		data_array_str = l.split()
		data_array = getFloatArray(data_array_str)
		
		res_bin = getResBin(data_array[0])
		
		if res_bin>=0:
			res_data[res_bin].append(data_array[1:])
			
	data_to_plot = getAverages(res_data)
	x_axis = []
	for i in range(1,len(data_to_plot[0])+1):
		x_axis.append(i*batch_size)
	print x_axis
	
	figure()
	#ax.xaxis.set_major_locator(MaxNLocator(integer=True))
	
	for d in data_to_plot:
		plt.plot(x_axis, d, "o-")
	
	plt.grid()
	plt.legend(['4-5A', '5-7A', '7-10A', '10-20A', '20-80A'])
	plt.title('Radiation damage analysis')
	plt.xlabel('Frame Number')
	plt.ylabel('Normalized Amplitude')
	
	plt.savefig(amp_pdf_file)
