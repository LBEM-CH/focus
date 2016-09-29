# !/usr/bin/env python
#############################################################################
#                                                                           #
# Title: Extract Particles from 2D Crystals									#
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
import glob
import os
import sys
import matplotlib.pyplot as plt
import matplotlib.cm as cm
import matplotlib.patches as patches
import EMAN2 as e2

def main():

	# Get arguments:
	folders = sys.argv[1]
	merge_dirfile = sys.argv[2]
	png_path = sys.argv[3]
	stack_path = sys.argv[4]
	stack_rootname = sys.argv[5]
	box_size = int(sys.argv[6])
	phaori_shift = np.array([float(sys.argv[7].split(',')[0]), float(sys.argv[7].split(',')[1])]) # How many degrees to offset the phase origins in order to get a protein in the center of the particle for a zero-tilt image
	apix = float(sys.argv[8]) # pixel size in Angstroems
	microscope_voltage = float(sys.argv[9]) # microscope voltage in kV
	microscope_cs = float(sys.argv[10]) # microscope spherical aberration in mm
	ampcontrast = 1-float(sys.argv[11])**2  # amplitude contrast of CTF (obtained here from phase contrast)
	magnification = float(sys.argv[12])
	sigcc = float(sys.argv[13])
	if sys.argv[14] == 'y':
		invert_micrograph = True
	else:
		invert_micrograph = False
	if sys.argv[15] == 'y':
		normalize_box = True
	else:
		normalize_box = False
	if sys.argv[16] == 'y':
		calculate_defocus_tilted = True
	else:
		calculate_defocus_tilted = False
	if sys.argv[17] == 'y':
		save_phase_flipped = True
	else:
		save_phase_flipped = False
	if sys.argv[18] == 'y':
		save_wiener_filtered = True
	else:
		save_wiener_filtered = False
	sigma = float(sys.argv[19]) # Sigma for normalization of the windowed images (if normalize_box == True)
	if sys.argv[20] == 'Defocus/Lattice':
		tiltgeom = ''
	elif sys.argv[20] == 'Defocus':
		tiltgeom = 'DEFOCUS_'
	elif sys.argv[20] == 'Lattice':
		tiltgeom = 'LATTICE_'
	elif sys.argv[20] == 'Merge':
		tiltgeom = 'MERGE_'
	if sys.argv[21] == 'Micrograph':
		ctfcor = True
		stack_rootname = stack_rootname + '_ctfcor'
	else:
		ctfcor = False
	if sys.argv[22] == 'y':
		save_pick_fig = True
	else:
		save_pick_fig = False
	# End arguments

	f = open(merge_dirfile,'r')
	img_dirs = f.readlines()
	f.close()

	# Constant parameters to be written on the .par file of this dataset:
	shx = 0.0
	shy = 0.0
	occ = 100.0
	logp = 0
	sig = 0.5 # This has nothing to do with the normalizaiton SIGMA!
	score = 0.0
	chg = 0.0

	n = 1
	idx = 0
	box_fail = 0
	phaori_err = 0

	N = len(img_dirs)

	f = open(stack_path+stack_rootname+'_1_r1.par', 'w+')

	for d in img_dirs:

		d = d.strip()
		imname = d.split('/')[-1]

		if ctfcor:

			try:

				mrc = glob.glob(folders+d+'/image_ctfcor.mrc')[0]
				bf = open(folders+d+'/image_ctfcor.box', 'w+')

			except:

				print ':: CTF-corrected micrograph not found for image %s!' % d
				print ':: '
				continue

		else:

			# First we look for the masked, zero-padded, normalized micrograph:
			try:

				# There might be some funny numbers appended to the file name so we have to look for the shortest one to get the right file:
				mrclist = glob.glob(folders+d+'/m'+imname+'*.mrc')
				lenlist = []
				for m in mrclist:
					lenlist.append(len(m))
				shortest_idx = np.argsort(lenlist)[0]
				# print mrclist[shortest_idx]
				mrc = mrclist[shortest_idx]
				bf = open(os.path.splitext(mrc)[0]+'.box', 'w+')
				# mrc = sorted(glob.glob(folders+d+'/m'+imname+'*.mrc'))[0]
				# bf = open(folders+d+'/m'+imname+'.box', 'w+')
				
			except:

				# If it doesn't exist, we try the unmasked, zero-padded, normalized micrograph:
				try:

					mrclist = glob.glob(folders+d+'/'+imname+'*.mrc')
					lenlist = []
					for m in mrclist:
						lenlist.append(len(m))
					shortest_idx = np.argsort(lenlist)[0]
					# print mrclist[shortest_idx]
					mrc = mrclist[shortest_idx]
					bf = open(os.path.splitext(mrc)[0]+'.box', 'w+')
					# mrc = sorted(glob.glob(folders+d+'/m'+imname+'*.mrc'))[0]
					# bf = open(folders+d+'/m'+imname+'.box', 'w+')

				except:

						# If neither exist we skip this image

						print ':: Problem with image %s!' % d
						print ':: '
						continue

		print ':: Now boxing unit cells of micrograph %d/%d.' % (n, N)
		print ':: '
		print mrc

		try:

			img = spx.get_image(mrc) # Read image

			if invert_micrograph:

				img = -1.0 * img

			imgarr = spx.EMNumPy.em2numpy(img)

			profile = glob.glob(folders+d+'/*profile.dat')[0]
			dat = ReadProfileDat(profile)
			ccmean = np.mean(dat[:,4])
			ccstd = np.std(dat[:,4])
			cc_thr = ccmean + sigcc * ccstd

			print ': Image average value: %.1f' % img['mean']
			print ': Image standard deviation: %.1f' % img['sigma']
			print ': '
			print ': CC scores average value: %.1f' % ccmean
			print ': CC scores standard deviation: %.1f' % ccstd
			print ': Only particles with CC score above %.1f will be picked.' % cc_thr
			print ': '

			# Get several values related to defocus, astigmatism and tilting:
			params = Read2dxCfgFile(folders+d+'/2dx_image.cfg')

			# Get the unit-cell vectors:
			if sum(params['PHAORI']) == 0:

				PhaOriX = 0
				PhaOriY = 0
				a = [0,0]
				b = [0,0]
			    
			else:
			    
				w = img.get_xsize()
				a,b = LatticeReciprocal2Real(params['u'], params['v'], w)

				# Convert from Numpy-array to list:
				a = [a[0], a[1]]
				b = [b[0], b[1]]

			x,y = CalculatePickingPositions(dat, a, b, w, params['PHAORI'], phaori_shift, params[tiltgeom+'TLTANG'])

			# Plot the picking profile:

			if save_pick_fig:

				meanimg = np.mean(imgarr)
				stdimg = np.std(imgarr)
				climimg = [meanimg - 2 * stdimg, meanimg + 2 * stdimg]

				fig1 = plt.figure()
				plt.imshow(imgarr, cmap=cm.gray, vmin=climimg[0], vmax=climimg[1])

				Axes1 = fig1.gca()

			# The following values are constant within each crystal:
			phi = 90.0 - params[tiltgeom+'TAXA']
			theta = params[tiltgeom+'TLTANG']
			psi = 270.0 - params[tiltgeom+'TLTAXIS']
			ang = params['AST_ANGLE']

			m = 0
			for i in np.arange(dat.shape[0]):

				try:

					# if dat[i,4] < cc_thr or np.isnan(x[i]) or np.isnan(y[i]):
					if dat[i,4] < cc_thr:

						if save_pick_fig:
							# Write red patch on image to be saved as .png describing the picking positions:
							Axes1.add_patch(patches.Circle((dat[i,2], dat[i,3]), edgecolor='red', facecolor='none', linewidth=0.5, radius=20))

					else:

						box = spx.Util.window(img,int(box_size),int(box_size),1,int(round(x[i])),int(round(y[i])))

						# Normalize box to zero mean and constant pre-defined sigma:
						if normalize_box:

							box = NormalizeStack([box], sigma)[0]

						if calculate_defocus_tilted and not ctfcor:

							RLDEF1,RLDEF2 = CalculateDefocusTilted(x[i], y[i], apix, params[tiltgeom+'TLTAXIS'], params[tiltgeom+'TLTANG'], params['DEFOCUS1'], params['DEFOCUS2'])

						else: 

							RLDEF1 = params['DEFOCUS1']
							RLDEF2 = params['DEFOCUS2']

						# Write .par file with the parameters for each particle in the dataset:
						print >>f, '      %d' % (idx+1),'  %.2f' % psi,'  %.2f' % theta,'    %.2f' % phi,'     %.2f' % shx,'      %.2f' % shy,'   %d' % magnification,'     %d' % n,'  %.2f' % RLDEF1,'  %.2f' % RLDEF2,'  %.2f' % ang,'  %.2f' % occ,'        %d' % logp,'     %.4f' % sig,'   %.2f' % score,'   %.2f' % chg

						# Write the picking information to the .box file:
						xbox = x[i] + w/2 - box_size/2
						ybox = y[i] + w/2 - box_size/2
						print >>bf, '%d' % xbox, '\t%d' % ybox, '\t%d' % box_size, '\t%d' % box_size

						# Write image to the particle stack:
						box.write_image(stack_path+stack_rootname+'.hdf', idx)

						if (save_phase_flipped or save_wiener_filtered) and not ctfcor:

							# Convert CTF parameters to SPARX convention:
							defocus = (RLDEF1+RLDEF2)/2
							ast = RLDEF1-RLDEF2
							if params['AST_ANGLE'] < 0.0:

								astang = 360.0 + params['AST_ANGLE']

							else:

								astang = params['AST_ANGLE']

							# Generate SPARX CTF object:
							p = [defocus * 1e-4, microscope_cs, microscope_voltage, apix, 0, ampcontrast * 100, ast * 1e-4, astang]
							
							spx.set_ctf(box, p)

							ctf = spx.generate_ctf(p)

						# Phase-flip the image:
						if save_phase_flipped and not ctfcor:

							boxctfcor = spx.filt_ctf(box, ctf,binary=1)
							if normalize_box:

								boxctfcor = NormalizeStack([boxctfcor], sigma)[0]

							boxctfcor.write_image(stack_path+stack_rootname+'_phase-flipped.hdf', idx)

						# Wiener-filter the image:
						if save_wiener_filtered and not ctfcor:

							boxctfcor = spx.filt_ctf(box, ctf,binary=0)
							if normalize_box:

								boxctfcor = NormalizeStack([boxctfcor], sigma)[0]
								
							boxctfcor.write_image(stack_path+stack_rootname+'_wiener-filtered.hdf', idx)

						if save_pick_fig:
							# Write green patch on image to be saved as .png describing the picking positions:
							Axes1.add_patch(patches.Circle((dat[i,2], dat[i,3]), edgecolor='lime', facecolor='none', linewidth=0.5, radius=20))

						m += 1
						idx += 1

				except RuntimeError:

					if save_pick_fig:
						# Write red patch on image to be saved as .png describing the picking positions:
						Axes1.add_patch(patches.Circle((dat[i,2], dat[i,3]), edgecolor='red', facecolor='none', linewidth=0.5, radius=20))

					print 'Failed to box unit cell (%d,%d) at position (%d,%d) in micrograph %d/%d!' % (dat[i,0], dat[i,1], int(round(x[i])), int(round(y[i])), n, N)
					box_fail += 1

			print ':: Boxed unit cells from micrograph %d/%d.' % (n, N)
			print ':: '
			print '<<@progress: %d >>' % round(n*100.0/N)

			if save_pick_fig:

				fig1.savefig(png_path+'mic_%.3d_' % n+imname+'_picking.png', dpi=300)
				plt.close(fig1)

			n += 1

		except RuntimeError:

			# print ':: PROBLEM WITH MICROGRAPH %d/%d!!! Maybe it was not found?' % (n, N)
			# print ':: '

			# print mrc
			print ':: PROBLEM WITH MICROGRAPH: '
			print ':: %s' % mrc
			print ':: Maybe it was not found?'

		except ValueError:

			# print ':: PROBLEM WITH CC PROFILE FOR IMAGE %d/%d!!!' % (n, N)
			# print ':: '

			# print mrc

			print ':: PROBLEM WITH CC PROFILE FOR IMAGE: '
			print ':: %s' % mrc

		bf.close()

		# n += 1


	print ':: Total boxed unit cells: %d' % idx
	print ':: Failed to box %d unit cells.' % box_fail

	f.close()

def Read2dxCfgFile(filepath):

	f = open(filepath,'r')
	lines = f.readlines()
	f.close()

	params = {}

	for l in lines:

		if l.startswith('set defocus ='):

			defocus_line = l.split('= ')[1].strip().split(',')
			params['DEFOCUS1'] = float(defocus_line[0][1:])
			params['DEFOCUS2'] = float(defocus_line[1])
			params['AST_ANGLE'] = float(defocus_line[2][:-1])

		if l.startswith('set TLTAXIS ='):

			params['TLTAXIS'] = float(l.split('= ')[1].strip()[1:-1])

		if l.startswith('set TLTANG ='):

			params['TLTANG'] = float(l.split('= ')[1].strip()[1:-1])

		if l.startswith('set TAXA ='):

			params['TAXA'] = float(l.split('= ')[1].strip()[1:-1])

		if l.startswith('set TANGL ='):

			params['TANGL'] = float(l.split('= ')[1].strip()[1:-1])

		if l.startswith('set TLTAXA ='):

			params['TLTAXA'] = float(l.split('= ')[1].strip()[1:-1])

			# Below we fetch the method-specific tilt geometries. If for some reason we don't find a value we assign them the corresponding default value found above.
		if l.startswith('set DEFOCUS_TLTAXIS ='):

			try:
				params['DEFOCUS_TLTAXIS'] = float(l.split('= ')[1].strip()[1:-1])
			except:
				params['DEFOCUS_TLTAXIS'] = params['TLTAXIS']

		if l.startswith('set DEFOCUS_TLTANG ='):

			try:
				params['DEFOCUS_TLTANG'] = float(l.split('= ')[1].strip()[1:-1])
			except:
				params['DEFOCUS_TLTANG'] = params['TLTANG']

		if l.startswith('set DEFOCUS_TAXA ='):

			try:
				params['DEFOCUS_TAXA'] = float(l.split('= ')[1].strip()[1:-1])
			except:
				params['DEFOCUS_TAXA'] = params['TAXA']

		if l.startswith('set DEFOCUS_TANGL ='):

			try:
				params['DEFOCUS_TANGL'] = float(l.split('= ')[1].strip()[1:-1])
			except:
				params['DEFOCUS_TANGL'] = params['TANGL']

		if l.startswith('set DEFOCUS_TLTAXA ='):

			try:
				params['DEFOCUS_TLTAXA'] = float(l.split('= ')[1].strip()[1:-1])
			except:
				params['DEFOCUS_TLTAXA'] = params['TLTAXA']

		if l.startswith('set MERGE_TLTAXIS ='):

			try:
				params['MERGE_TLTAXIS'] = float(l.split('= ')[1].strip()[1:-1])
			except:
				params['MERGE_TLTAXIS'] = params['TLTAXIS']

		if l.startswith('set MERGE_TLTANG ='):

			try:
				params['MERGE_TLTANG'] = float(l.split('= ')[1].strip()[1:-1])
			except:
				params['MERGE_TLTANG'] = params['TLTANG']

		if l.startswith('set MERGE_TAXA ='):

			try:
				params['MERGE_TAXA'] = float(l.split('= ')[1].strip()[1:-1])
			except:
				params['MERGE_TAXA'] = params['TAXA']

		if l.startswith('set MERGE_TANGL ='):

			try:
				params['MERGE_TANGL'] = float(l.split('= ')[1].strip()[1:-1])
			except:
				params['MERGE_TANGL'] = params['TANGL']

		if l.startswith('set MERGE_TLTAXA ='):

			try:
				params['MERGE_TLTAXA'] = float(l.split('= ')[1].strip()[1:-1])
			except:
				params['MERGE_TLTAXA'] = params['TLTAXA']

		if l.startswith('set LATTICE_TLTAXIS ='):

			try:
				params['LATTICE_TLTAXIS'] = float(l.split('= ')[1].strip()[1:-1])
			except:
				params['LATTICE_TLTAXIS'] = params['TLTAXIS']

		if l.startswith('set LATTICE_TLTANG ='):

			try:
				params['LATTICE_TLTANG'] = float(l.split('= ')[1].strip()[1:-1])
			except:
				params['LATTICE_TLTANG'] = params['TLTANG']

		if l.startswith('set LATTICE_TAXA ='):

			try:
				params['LATTICE_TAXA'] = float(l.split('= ')[1].strip()[1:-1])
			except:
				params['LATTICE_TAXA'] = params['TAXA']

		if l.startswith('set LATTICE_TANGL ='):

			try:
				params['LATTICE_TANGL'] = float(l.split('= ')[1].strip()[1:-1])
			except:
				params['LATTICE_TANGL'] = params['TANGL']

		if l.startswith('set LATTICE_TLTAXA ='):

			try:
				params['LATTICE_TLTAXA'] = float(l.split('= ')[1].strip()[1:-1])
			except:
				params['LATTICE_TLTAXA'] = params['TLTAXA']

		if l.startswith('set lattice ='):

			lattice = l.split('= ')[1].strip()[1:-1].split(',')
			params['u'] = [float(lattice[0]), float(lattice[1])]
			params['v'] = [float(lattice[2]), float(lattice[3])]

		if l.startswith('set phaori ='):

			phaori = l.split('= ')[1].strip()[1:-1].split(',')
			params['PHAORI'] = [float(phaori[0]), float(phaori[1])]
			
		if l.startswith('set realcell_local ='):

			realcell = l.split('= ')[1].strip()[1:-1].split(',')
			params['REALCELL'] = [float(realcell[0]), float(realcell[1])]

		if l.startswith('set rot90 ='):

			rot90_line = l.split('= ')[1].strip().split(',')
			params['rot90'] = rot90_line[0][1]

		if l.startswith('set rot180 ='):

			rot180_line = l.split('= ')[1].strip().split(',')
			params['rot180'] = rot180_line[0][1]

		if l.startswith('set sgnxch ='):

			sgnxch_line = l.split('= ')[1].strip().split(',')
			params['sgnxch'] = sgnxch_line[0][1]

		if l.startswith('set revhk ='):

			revhk_line = l.split('= ')[1].strip().split(',')
			params['revhk'] = revhk_line[0][1]

		if l.startswith('set revhnd ='):

			revhnd_line = l.split('= ')[1].strip().split(',')
			params['revhnd'] = revhnd_line[0][1]

		if l.startswith('set revxsgn ='):

			revxsgn_line = l.split('= ')[1].strip().split(',')
			params['revxsgn'] = revxsgn_line[0][1]

	return params

def LatticeReciprocal2Real(u,v,w):

	try:
	# Converts lattice vectors u,v in Fourier space to lattice vectors a,b in real space

	# void fullScreenImage::drawRealLattice(float lattice[2][2])

		# // u1,u2 and v1,v2 are the reciprocal lattice vectors.
		# // w is the image width for the square image.
		u1 = u[0]
		u2 = u[1]
		v1 = v[0]
		v2 = v[1]

		ASTR = np.sqrt(u1*u1+u2*u2);
		BSTR = np.sqrt(v1*v1+v2*v2);

		SINASTR=u2/ASTR;
		COSASTR=u1/ASTR;
		SINBSTR=v2/BSTR;
		COSBSTR=v1/BSTR;

		SINGMSTR=SINASTR*COSBSTR-COSASTR*SINBSTR;
		COSGMSTR=SINASTR*SINBSTR+COSASTR*COSBSTR;
		GAMMASTR=np.arctan2(SINGMSTR,COSGMSTR);

		if GAMMASTR >= 0.0:
		
			AA1 = -v2;
			AA2 =  v1;
			BB1 =  u2;
			BB2 = -u1;
		
		else:
		
			AA1 =  v2;
			AA2 = -v1;
			BB1 = -u2;
			BB2 =  u1;
		
		SINA=AA2/BSTR;
		COSA=AA1/BSTR;
		SINB=BB2/ASTR;
		COSB=BB1/ASTR;

		if (GAMMASTR < 0.0):

			GAMMASTR = -GAMMASTR;

		A=w/(ASTR*np.sin(GAMMASTR));
		B=w/(BSTR*np.sin(GAMMASTR));

		a1 =  A*COSA;
		a2 = A*SINA;
		b1 =  B*COSB;
		b2 = B*SINB;

		a = [a1, a2]
		b = [b1, b2]

		return a,b

	except RuntimeWarning:

		a = [0, 0]
		b = [0, 0]

	# a1,a2 and b1,b2 are now the real-space lattice vectors.

def ReadProfileDat(profilepath):
# The following function takes as argument the path to a .dat profile file generated by 2dx, containing data in columns arranged as follows:
# h k x_pos y_pos CC_value
#
# This file gives the (x,y) position of each unit cell in a 2D crystal, their Miller indices and corresponding CC value
# The output is a matrix of floats containing all the values.

	# Opens the .dat file for reading
	f = open(profilepath, 'r')

	# Read all lines to memory
	lines = f.readlines()

	# Closes the file
	f.close()

	# Discard header
	lines = lines[7:]
	L = len(lines)
	dat = np.zeros((L,5))
	# Strip all values in each line:
	for i in range(L):

		lines[i] = lines[i].split()

		# Convert each entry to float
		for j in range(5):

			dat[i,j] = float(lines[i][j])

	# Return a "list of lists" containing all the values
	return dat

def NormalizeStack(stack, sigma):
# Normalizes a stack of images to zero-mean and standard deviation given by sigma:

	stack_norm = []
	for i in range(len(stack)):

		zero_float = stack[i] - stack[i]['mean']
		zero_float.mult(sigma/zero_float['sigma'])
		stack_norm.append(zero_float)

	return stack_norm

def CalculateDefocusTilted(x, y, apix, TLTAXIS, TLTANG, DEFOCUS1, DEFOCUS2):

	# Find defocus and astigmatism values in particle position (we know them at the image center):
	rdist1 = np.sqrt(x**2 + y**2) # distance in pixels from image center to window center
	rbeta = np.arctan2(y, x) * 180.0 / np.pi # angle in degrees between the x-axis and the line connecting image center to window center
	rgamma = rbeta - TLTAXIS # angle between the line connecting image center to window center and tilt axis
	rdist2 = rdist1 * np.sin(rgamma * np.pi / 180.0) # distance in pixels between window center and closest point on tilt axis
	rdist3 = rdist2 * apix # distance in Angstroems
	RLDEF1 = DEFOCUS1 + rdist3 * np.tan(TLTANG * np.pi / 180.0)
	RLDEF2 = DEFOCUS2 + rdist3 * np.tan(TLTANG * np.pi / 180.0)

	return RLDEF1,RLDEF2

def CalculatePickingPositions(dat, a, b, w, PHAORI, phaori_shift, TLTANG):
# Here we compute the picking positions accounting for the phase origin shifts:

	PhaOriX = a[0] * (PHAORI[0] + phaori_shift[0] * np.cos(np.radians(TLTANG)))/360.0 + b[0] * (PHAORI[0] + phaori_shift[0] * np.cos(np.radians(TLTANG)))/360.0
	PhaOriY = a[1] * (PHAORI[1] + phaori_shift[1] * np.cos(np.radians(TLTANG)))/360.0 + b[1] * (PHAORI[1] + phaori_shift[1] * np.cos(np.radians(TLTANG)))/360.0
	# PhaOriX = a[0] * (PHAORI[1] + phaori_shift[0])/360.0 + b[0] * (PHAORI[1] + phaori_shift[1])/360.0
	# PhaOriY = a[1] * (PHAORI[1] + phaori_shift[0])/360.0 + b[1] * (PHAORI[1] + phaori_shift[1])/360.0

	PhaOriPx = [PhaOriX, PhaOriY]

	offset_x = 0.5 * ( a[0] + b[0] );
	offset_y = 0.5 * ( a[1] + b[1] );
	offset = [offset_x, offset_y]

	x = dat[:,2] - w/2 - PhaOriPx[0] + offset[0]
	y = dat[:,3] - w/2 - PhaOriPx[1] + offset[1]

	return x,y

if __name__ == '__main__':
	main()
