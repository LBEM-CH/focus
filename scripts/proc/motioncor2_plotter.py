from pylab import *
import os
import matplotlib.image as mpimg
import matplotlib.pyplot as plt

if __name__ == "__main__":
	
	if len(sys.argv) != 3:
		sys.exit("Usage: motioncor2_plotter.py <Data-File> <Output-File>")

	infile = sys.argv[1]
	outfile = sys.argv[2]
			
	data_file = open(infile)
	
	# Skip header 
	for i in range(0,8):
		data_file.readline()
		
	x=[]; y=[];
	
        for l in data_file
	        x.append(float(l.split(1)))
	        y.append(float(l.split(2)))

	for i in range(0,len(x)):
		print i," = ",x[i],",",y[i]

	xwidth=max(x)-min(x)
	ywidth=max(y)-min(y)
        if xwidth<ywidth:
		xwidth=ywidth

	xstart=(max(x)+min(x))/2-0.6*xwidth
	xend  =(max(x)+min(x))/2+0.6*xwidth
	ystart=(max(y)+min(y))/2-0.6*xwidth
	yend  =(max(y)+min(y))/2+0.6*xwidth

	print "xstart,end = ",xstart,xend,xwidth," ,   ystart,end = ",ystart,yend,ywidth
	plt.figure(figsize=(8,8))
	plt.subplot(111,autoscale_on=False,aspect='equal',xlim=[xstart,xend],ylim=[ystart,yend])
	
        plt.plot(x,y,'bo',markersize=3,linewidth=0.5)
        plt.plot(x,y,markersize=1,linewidth=0.5)
        plt.plot(x[0],y[0],'bo',markersize=10,linewidth=0.5)

        plt.title('Drift profile')

	plt.xlabel('X-Shift [A]')
	plt.ylabel('Y-Shift [A]')

	plt.grid(True)
	
	plt.savefig(outfile)

