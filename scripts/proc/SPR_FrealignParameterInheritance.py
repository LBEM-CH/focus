import numpy as np
import copy
import sys

def main():

	par_parent = sys.argv[1]
	par_child = sys.argv[2]

	parent_in = np.loadtxt(par_parent, comments='C')
	child_in = np.loadtxt(par_child, comments='C')
	child_out = copy.deepcopy(child_in)

	# We copy all parameters from the crystal average to their "child" particles, except for defocii and astigmatism which are kept at the particle-level.
	for i in parent_in[:,7]:

		# child_out[:,1:7] = parent_in[parent_in[:,7] == child_out[:,7],1:7]
		# child_out[:,11:] = parent_in[parent_in[:,7] == child_out[:,7],11:]
		child_out[child_out[:,7] == i,1:7] = parent_in[i-1,1:7]
		child_out[child_out[:,7] == i,11:] = parent_in[i-1,11:]

	# if recalculate_defocus:

	# 		child_out[:,8:10] = parent_in[parent_in[:,7] == child_out[:,7],8:10]

	# 		# Find defocus and astigmatism values in particle position (we know them at the image center):
	# 		rdist1 = np.sqrt(x[i]**2 + y[i]**2) # distance in pixels from image center to window center
	# 		rbeta = np.arctan2(y[i], x[i]) * 180.0 / np.pi # angle in degrees between the x-axis and the line connecting image center to window center

	# 		if consider_tltaxis:

	# 			rgamma = rbeta + (270.0 - child_out[:,1]) # angle between the line connecting image center to window center and tilt axis

	# 		else:

	# 			rgamma = rbeta + (270.0 - initial_in[initial_in[:,7] == child_out[:,7],1]) # angle between the line connecting image center to window center and tilt axis

	# 		rdist2 = rdist1 * np.sin(rgamma * np.pi / 180.0) # distance in pixels between window center and closest point on tilt axis
	# 		rdist3 = rdist2 * apix # distance in Angstroems
	# 		child_out[:,8] = child_out[:,8] + rdist3 * np.tan(child_out[:,2] * np.pi / 180.0)
	# 		child_out[:,9] = child_out[:,9] + rdist3 * np.tan(child_out[:,2] * np.pi / 180.0)

	np.savetxt(par_child, child_out, fmt=['%d', '%.2f', '%.2f', '%.2f', '%.2f', '%.2f', '%d', '%d', '%.2f', '%.2f', '%.2f', '%.2f', '%d', '%.4f', '%.2f', '%.2f'], delimiter='    ')

if __name__ == '__main__':
	main()