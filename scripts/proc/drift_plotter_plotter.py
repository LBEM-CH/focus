import os,sys
import matplotlib.pyplot as plt
import math

if __name__ == "__main__":
	
	if len(sys.argv) != 4:
		sys.exit("Usage: drift_plotter_plotter.py <Data-File-List> <Output-File> <pixelsize_in_Angstroems>")

	infile = sys.argv[1]
	outfile = sys.argv[2]
	angperpix = float(sys.argv[3])
			
	file_list = open(infile)

	files=[];
	for l in file_list:
		currentfile = l.split()
		files.append(currentfile)

	rcumlen=[[-100.0 for x in range(1000)] for y in range(len(files))];

	rmostframes = 0
	rlongest = 0.0

	for n in range(0,len(files)):
		currentfile = files[n]
		print "File: " + str(currentfile)
                data_file = open(currentfile[0])

                x=[]; y=[];

                for l in data_file:
                        data_split = l.split()
                        x.append(float(data_split[0])*angperpix)
                        y.append(float(data_split[1])*angperpix)

                xwidth=max(x)-min(x)
                ywidth=max(y)-min(y)
                rlen = math.sqrt(xwidth*xwidth + ywidth*ywidth)

		if rmostframes < len(x):
			rmostframes = len(x)

                xdiff=[]; ydiff=[];
                for i in range(0,len(x)-1):
                        xdiff.append(x[i]-x[i+1])
                        ydiff.append(y[i]-y[i+1])

                rlength=[];
                for i in range(0,len(x)-1):
                        rcurrent = math.sqrt(xdiff[i]*xdiff[i]+ydiff[i]*ydiff[i])
                        rlength.append(rcurrent)

		rcumlength=[];
		rcurrent = 0.0
                for i in range(0,len(x)-1):
			rcurrent=rcurrent+rlength[i]
			rcumlength.append(rcurrent)
                        if rcurrent > rlongest:
                                rlongest = rcurrent

                for i in range(0,len(x)-1):
                        # print i," of ",len(x)," = ",x[i],",",y[i],",   diff = ",rlength[i],",   cummulative = ",rcumlength[i]
			rcumlen[n][i]=rcumlength[i]
	

	xstart=0
	xend=rmostframes
	ystart=0.0
	yend=rlongest
	
	x=[n for n in range(rmostframes-1)];
	print "len = ",len(x)," and ",len(rcumlen)

        plt.figure(figsize=(8,8))
        # plt.subplot(111,autoscale_on=False,aspect='equal',xlim=[xstart,xend],ylim=[ystart,yend])
        plt.subplot(111,autoscale_on=False,xlim=[xstart,xend],ylim=[ystart,yend])

	for n in range(0,len(files)):
		y=[];
		for i in range(rmostframes-1):
			y.append(rcumlen[n][i])
	        plt.plot(x,y,'bo',markersize=3,linewidth=0.5)
		plt.plot(x,y,markersize=1,linewidth=0.5)
        
        plt.title('Drift profile')
        plt.xlabel('Frame Number')
        plt.ylabel('Cummulative Drift [px]')
        plt.grid(True)

        plt.savefig(outfile)

