import os,sys
import matplotlib.pyplot as plt

if __name__ == "__main__":
	
	if len(sys.argv) != 3:
		sys.exit("Usage: CTF_plotter.py <Data-File> <Output-File>")

	infile = sys.argv[1]
	outfile = sys.argv[2]
			
	data_file = open(infile)
	
	# Skip header 
	data_file.readline()
		
	res=[]; ctfsim=[]; EPA1=[]; EPA2=[]; CCC=[];
	recres=[];
	
	for l in data_file:
		data_split = l.split()
		res.append(float(data_split[0]))
		recres.append(1.0/float(data_split[0]))
		ctfsim.append(float(data_split[1]))
		EPA1.append(float(data_split[2]))
		EPA2.append(float(data_split[3]))
		CCC.append(float(data_split[4]))

	# for i in range(0,len(res)):
	#	print i," = ",res[i],ctfsim[i],EPA1[i],EPA2[i],CCC[i],recres[i]

	# xstart=res[0]
	# xend  =res[len(res)-1]
	xstart=recres[0]
	xend  =recres[len(res)-1]

	ystart=-0.2
	yend  =1.2

	# print "xstart,end = ",xstart,xend
	# print "ystart,end = ",ystart,yend

	plt.figure(figsize=(8,8))
	plt.subplot(111)
	plt.axis([xstart,xend,ystart,yend])
	# plt.xscale('log')
	
	plt.plot(recres,ctfsim,markersize=1,linewidth=0.5)
	plt.plot(recres,EPA2,markersize=2,linewidth=0.5)

	plt.title('gctf plot')

	plt.xlabel('1/Resolution [1/A]  (Range: '+str(round(res[0],1))+':'+str(round(res[len(res)-1],1))+'A)')
	plt.ylabel('EPA ( ln|F| )')

	plt.grid(True)
	
	plt.savefig(outfile)

