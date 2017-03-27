import os,sys
import matplotlib.pyplot as plt
import math

def basic_linear_regression(x, y):
	# taken from http://jmduke.com/posts/basic-linear-regressions-in-python/
	length = len(x)
	sum_x = sum(x)
	sum_y = sum(y)
	sum_x_squared = sum(map(lambda a: a * a, x))
	sum_of_products = sum([x[i] * y[i] for i in range(length)])
	a = (sum_of_products - (sum_x * sum_y) / length) / (sum_x_squared - ((sum_x ** 2) / length))
	b = (sum_y - a * sum_x) / length
	return a, b

if __name__ == "__main__":
	
	if len(sys.argv) != 4:
		sys.exit("Usage: motioncor2_plotter.py <Data-File> <Output-File> <Output text file>")

	infile = sys.argv[1]
	outfile = sys.argv[2]
	txtfile = sys.argv[3]
			
	data_file = open(infile)
	
	# Skip the Skipping:
	## Skip header 
	# for i in range(0,8):
	#	data_file.readline()
	# Skip fist point
	# data_file.readline()

	x=[]; y=[];
	
	for l in data_file:
		data_split = l.split()
		x.append(float(data_split[0]))
		y.append(float(data_split[1]))

	for i in range(0,len(x)):
		print i," of ",len(x)," = ",x[i],",",y[i]

	xwidth=max(x)-min(x)
	ywidth=max(y)-min(y)
	rlen = math.sqrt(xwidth*xwidth + ywidth*ywidth)

	if xwidth<ywidth:
		xwidth=ywidth

	print "xmin,xmax = ",min(x),max(x),", ymin,ymax = ",min(y),max(y)
	print "::drift length = ",rlen," nm"

	xstart=(max(x)+min(x))/2-0.6*xwidth
	xend  =(max(x)+min(x))/2+0.6*xwidth
	ystart=(max(y)+min(y))/2-0.6*xwidth
	yend  =(max(y)+min(y))/2+0.6*xwidth

	print "xstart,end = ",xstart,xend,xwidth," ,   ystart,end = ",ystart,yend,ywidth

	xdiff=[]; ydiff=[];
	rlongest = 0.0
	for i in range(0,len(x)-1):
		xdiff.append(x[i]-x[i+1])
		ydiff.append(y[i]-y[i+1])

	rlength=[];
	for i in range(0,len(x)-1):
		rcurrent = math.sqrt(xdiff[i]*xdiff[i]+ydiff[i]*ydiff[i])
		rlength.append(rcurrent)
		if rcurrent > rlongest:
			rlongest = rcurrent
            
	print "::longest drift step = ",rlongest," nm"

	sum_x = 0.0
	sum_y = 0.0
	sum_x2 = 0.0
	sum_xy = 0.0
	for i in range(0,len(rlength)):
		print i," of ",len(rlength)," = ",rlength[i]
		sum_x = sum_x + i
		sum_y = sum_y + rlength[i]
		sum_x2 = sum_x2 + i*i
		sum_xy = sum_xy + i*rlength[i]
	print "len(rlength) = ",len(rlength)
	print "sum_x = ",sum_x,",  sum_y = ",sum_y,",  sum_x2 = ",sum_x2,",  sum_xy = ",sum_xy
	slope = (sum_xy - (sum_x * sum_y) / len(rlength)) / (sum_x2 - ((sum_x ** 2) / len(rlength)))
	offset = (sum_y - slope * sum_x) / len(rlength)
	print "::offset of drift = ",offset
	print "::slope  of drift = ",slope

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
	line = "set import_drift_longest = " + str(10.0*rlongest) + "\n"
	data_file_out.write(line)
	line = "set import_drift_deceleration = " + str(-10.0*slope) + "\n"
	data_file_out.write(line)
	data_file_out.close()

