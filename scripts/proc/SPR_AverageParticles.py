#!/usr/bin/env python
#############################################################################
#                                                                           #
# Title: Average Particles from 2D Crystals									#
#                                                                           #
# (C) 2dx.org, GNU Plublic License.                                         #
#                                                                           #
# Created..........: 29/07/2016                                             #
# Last Modification: 29/07/2016                                             #
# Author...........: Ricardo Righetto                                       #
#                                                                           #
#############################################################################
import sparx as spx
import numpy as np
import matplotlib.pyplot as plt
import sys
import ioMRC
import focus_utilities as util

def main():

	# Get arguments:
	stack_path = sys.argv[1]
	stack_rootname = sys.argv[2]+'_'+sys.argv[3]
	if sys.argv[4] == 'y':
		do_frc = True
	else:
		do_frc = False
	frc_folder = sys.argv[5]
	sigma = float(sys.argv[6]) # Sigma for normalization of the windowed images (if normalize_box == True)
	apix = float(sys.argv[7]) # pixel size in Angstroems
	thr = float(sys.argv[8]) # pixel size in Angstroems
	n_threads = int(sys.argv[9])
	if n_threads < 1:
		n_threads = 1
	this_thread = int(sys.argv[10])
	# End arguments

	stack_file = stack_path+stack_rootname+'.mrcs'

	f = open(stack_path+stack_rootname+'_crystal-avg_1_r1-%.4d.par' % this_thread, 'w+')

	# first = spx.EMData()
	# first.read_image(stack_file, 0)
	header = ioMRC.readMRCHeader( stack_file )

	par = np.loadtxt(stack_path+stack_rootname+'_1_r1.par', comments='C')
	labels = par[:,7]
	X = np.unique(labels)
	XN = len(X)

	batch_size = round(float(XN)/n_threads)
	first_img = int((this_thread-1) * batch_size)

	if this_thread < n_threads:

		last_img = int(first_img + batch_size)

	else:

		last_img = XN

	X = X[first_img:last_img]

	n = first_img + 1

	print '\nJob %d/%d averaging particles from crystals %d to %d...\n' % (this_thread, n_threads, n, last_img)

	prog = 0.0
	j = 1
	for x in X:

		print '::Averaging particles from crystal %d/%d...' % (x, XN)

		img_list = np.where(labels == x)[0]

		# avg = spx.EMData(first.get_xsize(),first.get_ysize())
		#  ioMRC header is Z,Y,X:
		avg = np.zeros( [header['dimensions'][2], header['dimensions'][1]] )

		if do_frc:

			plt.figure()

			# odd = spx.EMData(first.get_xsize(),first.get_ysize())
			# even = spx.EMData(first.get_xsize(),first.get_ysize())
			odd = np.zeros( [header['dimensions'][2], header['dimensions'][1]] )
			even = np.zeros( [header['dimensions'][2], header['dimensions'][1]] )

		for i in img_list:

			# img = spx.EMData()
			# img.read_image(stack_file, int(i))
			img = ioMRC.readMRC( stack_file, idx=i )

			avg += img

			if do_frc:

				if np.mod(i,2) == 1:

					odd += img

				else:

					even += img

	# if normalize_box:

		# box = NormalizeStack([box], sigma)[0]
		avg = util.NormalizeImg( avg, std=sigma )
		# avg = NormalizeStack([avg], sigma)[0]

		# avg.write_image(stack_path+stack_rootname+'_crystal-avg-%.4d.mrcs' % this_thread, j-1)
		ioMRC.writeMRC( avg, stack_path+stack_rootname+'_crystal-avg-%.4d.mrcs' % this_thread, dtype='float32', idx=j-1 )


		# Write .par file with the parameters for each particle in the dataset:
		# print >>f, '      %d' % (x),'  %.2f' % par[img_list[0],1],'  %.2f' % par[img_list[0],2],'    %.2f' % par[img_list[0],3],'     %.2f' % par[img_list[0],4],'      %.2f' % par[img_list[0],5],'   %d' % par[img_list[0],6],'     %d' % par[img_list[0],7],'  %.2f' % par[img_list[0],8],'  %.2f' % par[img_list[0],9],'  %.2f' % par[img_list[0],10],'  %.2f' % par[img_list[0],11],'        %d' % par[img_list[0],12],'     %.4f' % par[img_list[0],13],'   %.2f' % par[img_list[0],14],'   %.2f' % par[img_list[0],15]
		print >>f, '      %d' % (j),'  %.2f' % par[img_list[0],1],'  %.2f' % par[img_list[0],2],'    %.2f' % par[img_list[0],3],'     %.2f' % par[img_list[0],4],'      %.2f' % par[img_list[0],5],'   %d' % par[img_list[0],6],'     %d' % par[img_list[0],7],'  %.2f' % par[img_list[0],8],'  %.2f' % par[img_list[0],9],'  %.2f' % par[img_list[0],10],'  %.2f' % par[img_list[0],11],'        %d' % par[img_list[0],12],'     %.4f' % par[img_list[0],13],'   %.2f' % par[img_list[0],14],'   %.2f' % par[img_list[0],15]

		if do_frc:

			frc = spx.fsc(odd, even)

			plt.plot(np.array(frc[0][1:])/apix,frc[1][1:])

			yvalues = [i/10.0 for i in np.arange(np.round(np.min(frc[1][1:]))*10.0,11)]
			yvalues.append(thr)

			plt.title('Fourier Ring Correlation - TLTANG = %.1f' % par[img_list[0],2])
			plt.ylabel('FRC')
			plt.xlabel('Spatial frequency (1/A)')
			# plt.ylim(0.0,1.0)
			plt.yticks(yvalues)
			plt.grid()
			plt.savefig(frc_folder+'crystal_'+'%.3d' % x+'_'+sys.argv[3]+'_FRC.png', dpi=300)
			plt.close()

		j += 1

		# Report progress to the GUI:
		prog += 90.0/XN
		if prog >= 1.0:
			print '<<@progress: +%d>>' % round(prog)
			prog -= np.floor(prog)

	# print ':: '


# def NormalizeStack(stack, sigma):
# # Normalizes a stack of images to zero-mean and standard deviation given by sigma:

# 	stack_norm = []
# 	for i in range(len(stack)):

# 		zero_float = stack[i] - stack[i]['mean']
# 		zero_float.mult(sigma/zero_float['sigma'])
# 		stack_norm.append(zero_float)

# 	return stack_norm

if __name__ == '__main__':
	main()