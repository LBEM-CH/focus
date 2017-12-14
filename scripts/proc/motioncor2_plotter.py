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
	
	if len(sys.argv) != 5:
		sys.exit("Usage: motioncor2_plotter.py <Data-File> <Output-File> <Output text file> <pixelsize_in_Angstroems>")

	infile = sys.argv[1]
	outfile = sys.argv[2]
	txtfile = sys.argv[3]
	angperpix = float(sys.argv[4])
			
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
		x.append(float(data_split[0])*angperpix)
		y.append(float(data_split[1])*angperpix)

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
            
        rlongest = rlongest / 10.0
	print "::longest drift step = ",rlongest," nm"

        rdist_sum = 0.0
        for i in range(0,len(x)-2):
                rmid_x = (x[i] + x[i+2]) / 2.0
                rmid_y = (y[i] + y[i+2]) / 2.0
                rdist_x = x[i+1] - rmid_x
                rdist_y = y[i+1] - rmid_y
                rdist = math.sqrt(rdist_x ** 2 + rdist_y ** 2)
                rdist_sum = rdist_sum + rdist
        if sum(rlength) == 0:
        	rjitter = 0
        else:
        	rjitter = 1000.0 * rdist_sum / sum(rlength)
	print "::jitter of drift = ",rjitter

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
	if (rlength == 0 or sum_x == 0):
		slope = 0
	else:
		slope = (sum_xy - (sum_x * sum_y) / len(rlength)) / (sum_x2 - ((sum_x ** 2) / len(rlength)))
	
	if len(rlength) == 0:
		offset = 0
	else:
		offset = (sum_y - slope * sum_x) / len(rlength)
	print "::offset of drift = ",offset
	print "::slope  of drift = ",slope

	plt.figure(figsize=(8,8))
	if (xwidth <> 0 and ywidth <> 0):
		plt.subplot(111,autoscale_on=False,aspect='equal',xlim=[xstart,xend],ylim=[ystart,yend])
	else:
		plt.subplot(111,autoscale_on=False,aspect='equal',xlim=[xstart,xend+1],ylim=[ystart,yend+1])
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
	line = "set import_drift_longest = " + str(rlongest) + "\n"
	data_file_out.write(line)
	line = "set import_drift_deceleration = " + str(-10.0*slope) + "\n"
	data_file_out.write(line)
	line = "set import_drift_jitter = " + str(rjitter) + "\n"
	data_file_out.write(line)
	data_file_out.close()

