import os,sys
import matplotlib.pyplot as plt
import math

if __name__ == "__main__":
	
	if len(sys.argv) != 4:
		sys.exit("Usage: motioncor2_plotter.py <Data-File> <Output-File> <Output text file>")

	infile = sys.argv[1]
	outfile = sys.argv[2]
	txtfile = sys.argv[3]
			
	data_file = open(infile)
	
	# Skip header 
	for i in range(0,8):
		data_file.readline()
		
	x=[]; y=[];
	
        for l in data_file:
                data_split = l.split()
                x.append(float(data_split[0]))
                y.append(float(data_split[1]))

	for i in range(0,len(x)):
		print i," = ",x[i],",",y[i]

	xwidth=max(x)-min(x)
	ywidth=max(y)-min(y)
        rlen = math.sqrt(xwidth*xwidth + ywidth*ywidth)

        if xwidth<ywidth:
		xwidth=ywidth

        print "xmin,xmax = ",min(x),max(x),", ymin,ymax = ",min(y),max(y)
        print "::drift length [A] = ",rlen


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

	print "Opening ", txtfile
	data_file_out = open(txtfile,'w')
	# print "i,IFRAMS=",iframe,IFRAMS,"  mean_start=",p.mean_start_x,p.mean_start_y,"  mean_end=",p.mean_end_x,p.mean_end_y
	line = "set import_drift = " + str(rlen) + "\n"
	data_file_out.write(line)
	data_file_out.close()

