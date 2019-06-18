#!/usr/bin/env python

import sys
import numpy as np
import matplotlib.pyplot as plt

def main():

	fsc_dir = sys.argv[1]
	thr = float(sys.argv[2])
	parfiles = sys.argv[3:-1]

	fsc_ensemble = np.zeros((len(parfiles),2))
	part_fsc_ensemble = np.zeros((len(parfiles),2))
	j = 0

	for par in parfiles:

		f = open(par,'r')
		lines = f.readlines()
		f.close()

		i = 1
		for line in reversed(lines):

			values = line.split()

			if len(values) >= 2:

				if values[1] == 'Average':

					highestres_idx = i+1 # Line index of the highest resolution bin present in the FSC

				if values[1] == 'NO.':

					lowestres_idx = i-1 # Line index of the lowest resolution bin present in the FSC
					break
		 
			i += 1

		# Convert the reversed line indices to normal order:
		totlines = len(lines)

		lowestres_idx = totlines - lowestres_idx
		highestres_idx = totlines - highestres_idx

		i = lowestres_idx
		i_fsc = 0
		i_part_fsc = 0

		try:

			while True:

				if float(lines[i+1].split()[5]) < thr and i_fsc == 0:

					line_fsc = lines[i].split()
					print( ':: '+par+' FSC = %s @ %s A' % (line_fsc[5], line_fsc[2]) )
					fsc_ensemble[j,:] = [float(line_fsc[5]), float(line_fsc[2])]
					i_fsc = 1

				if float(lines[i+1].split()[6]) < thr and i_part_fsc == 0:

					line_part_fsc = lines[i].split()
					print( ':: '+par+' Part_FSC = %s @ %s A\n' % (line_part_fsc[6], line_part_fsc[2]) )
					part_fsc_ensemble[j,:] = [float(line_part_fsc[6]), float(line_part_fsc[2])]
					i_part_fsc = 1

				if i_fsc and i_part_fsc:

					j += 1

					break

				i += 1

		except IndexError:

			line_part_fsc = lines[highestres_idx].split()
			print( ':: '+par+' Part_FSC = %s @ %s A\n' % (line_part_fsc[6], line_part_fsc[2]) )
			part_fsc_ensemble[j,:] = [float(line_part_fsc[6]), float(line_part_fsc[2])]

		# Plot the FSC curves properly:
		res = []
		fsc = []
		part_fsc = []
		i = lowestres_idx
		while i <= highestres_idx:

			res.append(float(lines[i].split()[3]))
			fsc.append(float(lines[i].split()[5]))
			part_fsc.append(float(lines[i].split()[6]))

			i += 1

		yvalues = [i/10.0 for i in np.arange(11)]
		yvalues.append(thr)

		plt.figure()
		plt.plot(res,fsc)
		plt.title('Fourier Shell Correlation')
		plt.ylabel('FSC')
		plt.xlabel('Spatial frequency (1/A)')
		plt.ylim(-1.0,1.0)
		plt.yticks(yvalues)
		plt.grid()
		plt.savefig(fsc_dir+'/'+par.split('.')[0]+'_fsc.png',dpi=300)
		plt.close()

		plt.figure()
		plt.plot(res,part_fsc)
		plt.title('Fourier Shell Correlation')
		plt.ylabel('Part. FSC')
		plt.xlabel('Spatial frequency (1/A)')
		plt.ylim(-1.0,1.0)
		plt.yticks(yvalues)
		plt.grid()
		plt.savefig(fsc_dir+'/'+par.split('.')[0]+'_part-fsc.png',dpi=300)
		plt.close()

		plt.figure()
		plt.plot(res,fsc,res,part_fsc)
		plt.title('Fourier Shell Correlation')
		plt.ylabel('FSC')
		plt.xlabel('Spatial frequency (1/A)')
		plt.ylim(-1.0,1.0)
		plt.yticks(yvalues)
		plt.grid()
		plt.legend(['FSC','Part. FSC'])
		plt.savefig(fsc_dir+'/'+par.split('.')[0]+'_both-fsc.png',dpi=300)
		plt.close()

	if len(parfiles) > 1:

		print( ':: \nMean ensemble FSC = %.3f @ %.2f A' % (np.mean(fsc_ensemble[:,0]), np.mean(fsc_ensemble[:,1])) )
		print( ':: Mean ensemble Part_FSC = %.3f @ %.2f A' % (np.mean(part_fsc_ensemble[:,0]), np.mean(part_fsc_ensemble[:,1])) )
		print( ':: \nMedian ensemble FSC = %.3f @ %.2f A' % (np.median(fsc_ensemble[:,0]), np.median(fsc_ensemble[:,1])) )
		print( ':: Median ensemble Part_FSC = %.3f @ %.2f A' % (np.median(part_fsc_ensemble[:,0]), np.median(part_fsc_ensemble[:,1])) )

if __name__ == '__main__':
	main()