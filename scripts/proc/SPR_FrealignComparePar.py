import numpy as np
import matplotlib.pyplot as plt
import sys

def main():
	
	par_initial = sys.argv[1]
	par_final = sys.argv[2]
	diff_out = sys.argv[3]
	plot_dir = sys.argv[4]

	euler = ['TLTAXIS', 'TLTANG', 'TAXA']

	ini = np.loadtxt(par_initial, comments='C')

	fin = np.loadtxt(par_final, comments='C')

	print np.min(ini[:,2])
	print np.max(ini[:,2])

	ini[:,1:4] = np.radians(ini[:,1:4])

	fin[:,1:4] = np.radians(fin[:,1:4])

	sin = np.sin(ini[:,1:4]-fin[:,1:4])
	cos = np.cos(ini[:,1:4]-fin[:,1:4])

	diff = np.zeros((np.shape(ini)[0],3))
	mediandiff = np.zeros((1,3))
	meandiff = np.zeros((1,3))
	stddiff = np.zeros((1,3))

	for i in np.arange(3):

		diff[:,i] = np.degrees(np.arctan2(sin[:,i], cos[:,i]))
		meandiff[0,i] = np.degrees(np.arctan2(np.mean(np.sin(np.radians(diff[:,i]))), np.mean(np.cos(np.radians(diff[:,i])))))
		stddiff[0,i] = np.degrees(np.arctan2(np.std(np.sin(np.radians(diff[:,i]))), np.std(np.cos(np.radians(diff[:,i])))))
		mediandiff[0,i] = np.degrees(np.arctan2(np.median(np.sin(np.radians(np.abs(diff[:,i])))), np.median(np.cos(np.radians(np.abs(diff[:,i]))))))

		print '%s difference: %.2f mean, %.2f standard deviation' % (euler[i], meandiff[0,i], stddiff[0,i])
		print '%s max abs. difference: %.2f' % (euler[i], np.max(np.abs(diff[:,i])))
		print '%s min abs. difference: %.2f' % (euler[i], np.min(np.abs(diff[:,i])))
		print '%s median abs. difference: %.2f' % (euler[i], mediandiff[0,i])

		plt.clf()
		plt.hist(diff[:,i],bins=120,range=[-180.0,+180.0])
		plt.title(euler[i]+' change in pre-refinement')
		plt.xlabel('Change [degrees]')
		plt.ylabel('# of crystals')
		plt.savefig(plot_dir+'/'+euler[i]+'_change_hist.png',dpi=300)

	# Here we sum PHI and PSI because they are rotations about the same axis:
	deltasig = np.degrees(np.arccos(np.sin(ini[:,2]) * np.sin(fin[:,2]) + np.cos(ini[:,2]) * np.cos(fin[:,2]) * np.cos(fin[:,1]+fin[:,3]-ini[:,1]-ini[:,3])))
	plt.clf()
	plt.hist(deltasig,bins=180,range=[0,+180.0])
	plt.title('Angular change during pre-refinement (normal vector)')
	plt.xlabel('Change [degrees]')
	plt.ylabel('# of crystals')
	plt.savefig(plot_dir+'/'+'angular_change_hist.png',dpi=300)

	shxabs = np.abs(fin[:,4])
	print 'SHX difference: %.2f mean, %.2f standard deviation' % (np.mean(fin[:,4]), np.std(fin[:,4]))
	print 'SHX max abs. difference: %.2f' % (np.max(shxabs))
	print 'SHX min abs. difference: %.2f' % (np.min(shxabs))
	print 'SHX median abs. difference: %.2f' % (np.median(shxabs))

	plt.clf()
	plt.hist(fin[:,4],bins=25)
	plt.title('SHX change during pre-refinement')
	plt.xlabel('Change [Angstroems]')
	plt.ylabel('# of crystals')
	plt.savefig(plot_dir+'/'+'SHX_change_hist.png',dpi=300)

	shyabs = np.abs(fin[:,5])
	print 'SHY difference: %.2f mean, %.2f standard deviation' % (np.mean(fin[:,5]), np.std(fin[:,5]))
	print 'SHY max abs. difference: %.2f' % (np.max(shyabs))
	print 'SHY min abs. difference: %.2f' % (np.min(shyabs))
	print 'SHY median abs. difference: %.2f' % (np.median(shyabs))

	plt.clf()
	plt.hist(fin[:,5],bins=25)
	plt.title('SHY change during pre-refinement')
	plt.xlabel('Change [Angstroems]')
	plt.ylabel('# of crystals')
	plt.savefig(plot_dir+'/'+'SHY_change_hist.png',dpi=300)

	shxyabs = np.sqrt(np.sum(fin[:,4:6]**2,1))
	print 'Translational difference: %.2f mean, %.2f standard deviation' % (np.mean(shxyabs), np.std(shxyabs))
	print 'Translational max abs. difference: %.2f' % (np.max(shxyabs))
	print 'Translational min abs. difference: %.2f' % (np.min(shxyabs))
	print 'Translational median abs. difference: %.2f' % (np.median(shxyabs))

	plt.clf()
	plt.hist(shxyabs,bins=50)
	plt.title('Translational change during pre-refinement')
	plt.xlabel('Change [Angstroems]')
	plt.ylabel('# of crystals')
	plt.savefig(plot_dir+'/'+'translational_change_hist.png',dpi=300)

	np.savetxt(diff_out, diff, fmt=['%.2f', '%.2f', '%.2f'], delimiter='\t')

if __name__ == '__main__':
	main()