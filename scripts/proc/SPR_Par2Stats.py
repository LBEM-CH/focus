#!/usr/bin/env python

import sys
import numpy as np
import matplotlib.pyplot as plt

def main():

	parfiles = sorted(sys.argv[2:])
	stats_dir = sys.argv[1]

	avg_sc = []
	std_sc = []
	avg_chg = []
	avg_ll = []
	cycles = []
	data = np.zeros((len(parfiles),8))
	i = 0
	for par in parfiles:


		name = par.split('_')
		c = int(name[1]) # Cycle
		r = int(name[2].split('.')[0][-1]) # Reference

		if i == 0:

			cycles.append(c)

		if (cycles[-1] != 0 and c != cycles[-1]):

			sel = data[:,0]==cycles[-1]

			avg_ll.append(sum(data[sel,3]*data[sel,4])/100.0)
			avg_sc.append(sum(data[sel,3]*data[sel,5])/100.0)
			std_sc.append(sum(data[sel,3]*data[sel,6])/100.0)
			avg_chg.append(sum(data[sel,3]*data[sel,7])/100.0)

			print ':: \nCycle %d: N = %d, Avg. OCC = %.2f, Avg. LL = %.2f, Avg. Score = %.2f, Std. Score = %.2f, Avg. Change = %.2f \n' % (cycles[-1], n, 100.0, avg_ll[-1], avg_sc[-1], std_sc[-1], avg_chg[-1])

			cycles.append(c)

		ptcls = np.loadtxt(par, comments='C')
		n = ptcls.shape[0]
		occ = sum(ptcls[:,11])
		logp = sum(ptcls[:,11] * ptcls[:,12])/occ
		score = sum(ptcls[:,11] * ptcls[:,14])/occ
		std_score = np.sqrt(sum(ptcls[:,11] * (ptcls[:,14] - score)**2)/occ)
		change = sum(ptcls[:,11] * ptcls[:,15])/occ


		data[i,:] = [c, r, int(occ/100.0), occ/n, logp, score, std_score, change]

		print ':: %s: N = %d, Avg. OCC = %.2f, Avg. LL = %.2f, Avg. Score = %.2f, Std. Score = %.2f, Avg. Change = %.2f' % (par, data[i,2], data[i,3], data[i,4], data[i,5], data[i,6], data[i,7])

		if par == parfiles[-1]:

			sel = data[:,0]==cycles[-1]

			avg_ll.append(sum(data[sel,3]*data[sel,4])/100.0)
			avg_sc.append(sum(data[sel,3]*data[sel,5])/100.0)
			std_sc.append(sum(data[sel,3]*data[sel,6])/100.0)
			avg_chg.append(sum(data[sel,3]*data[sel,7])/100.0)

			print ':: \nCycle %d: N = %d, Avg. OCC = %.2f, Avg. LL = %.2f, Avg. Score = %.2f, Std. Score = %.2f, Avg. Change = %.2f \n' % (cycles[-1], n, 100.0, avg_ll[-1], avg_sc[-1], std_sc[-1], avg_chg[-1])

		i += 1

	if len(parfiles) > 1:

		plt.figure()
		plt.scatter(cycles,avg_sc)
		plt.title('FREALIGN Avg. Score vs. Cycles')
		plt.ylabel('Avg. Score')
		plt.xlabel('Cycle #')
		plt.grid()
		plt.savefig(stats_dir+'/'+par.split('_')[0]+'_score-vs-cycle.png',dpi=300)
		plt.close()

		plt.figure()
		plt.scatter(cycles,avg_chg)
		plt.title('FREALIGN Avg. Change vs. Cycles')
		plt.ylabel('Avg. Change')
		plt.xlabel('Cycle #')
		plt.grid()
		plt.savefig(stats_dir+'/'+par.split('_')[0]+'_change-vs-cycle.png',dpi=300)
		plt.close()

		sort_idx = np.argsort(cycles)
		cycles_sorted = np.array(cycles)[sort_idx]

		plt.figure()
		plt.scatter(cycles_sorted,np.cumsum(np.array(avg_chg)[sort_idx]))
		plt.title('FREALIGN Cummulative Change vs. Cycles')
		plt.ylabel('Cummulative Avg. Change')
		plt.xlabel('Cycle #')
		plt.grid()
		plt.savefig(stats_dir+'/'+par.split('_')[0]+'_cummulchange-vs-cycle.png',dpi=300)
		plt.close()

		plt.figure()
		plt.scatter(cycles,avg_ll)
		plt.title('FREALIGN Avg. Log-Likelihood vs. Cycles')
		plt.ylabel('Avg. LL')
		plt.xlabel('Cycle #')
		plt.grid()
		plt.savefig(stats_dir+'/'+par.split('_')[0]+'_LL-vs-cycle.png',dpi=300)
		plt.close()

if __name__ == '__main__':
	main()