import os,sys
import matplotlib.pyplot as plt
import math
import numpy as np

if __name__ == "__main__":
	
	if len(sys.argv) != 9:
		sys.exit("Usage: drift_plotter_plotter.py <Data-File-List> <Output-File> <pixelsize_in_Angstroems> <framenumber> <driftrange> <dots> <centerframe> <average>")

	infile = sys.argv[1]
	outfile = sys.argv[2]
	angperpix = float(sys.argv[3])
	framenumber = int(sys.argv[4])
	driftrange = float(sys.argv[5])
	dots = str(sys.argv[6])
	centerframe = int(sys.argv[7])
	average = int(sys.argv[8])
			
	file_list = open(infile)

	files=[];
	for l in file_list:
		currentfile = l.split()
		files.append(currentfile)

	rcumlen=[[-1.0e-10 for x in range(1000)] for y in range(len(files)+1)];

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

		rcenterlength = 0.0
                for i in range(0,centerframe):
			rcenterlength=rcenterlength+rlength[i]
		if centerframe < 2:
			rcenterlength=0.0

		rcumlength=[];
		rcurrent = 0.0
                for i in range(0,len(x)-1):
			rcurrent=rcurrent+rlength[i]
			rcumlength.append(rcurrent)
			if n == 0:
				if i == 0:
					raverage=np.zeros(10*len(x),dtype='float64');
					icounter=np.zeros(10*len(x),dtype='int');
				raverage[i]=(rcurrent-rcenterlength)
				icounter[i]=1
			else:
				#print "i=",i
				raverage[i]=raverage[i]+rcurrent-rcenterlength
				icounter[i]=icounter[i]+1

                        if rcurrent > rlongest:
                                rlongest = rcurrent

		rcumlen[n][0]=0.0
                for i in range(0,len(x)-1):
                        # print i," of ",len(x)," = ",x[i],",",y[i],",   diff = ",rlength[i],",   cummulative = ",rcumlength[i]
			rcumlen[n][i+1]=rcumlength[i]-rcenterlength
	
	raver=[];
	raver.append(0.0)
        for i in range(0,rmostframes-1):
		raver.append(raverage[i]/icounter[i])

	if centerframe < 2: 
		xstart=0
	else:
		xstart=1
	if framenumber < 1:
		xend=rmostframes
	else:
		xend=framenumber
	print "::Framenumber for plot is ",framenumber,".  Using ",xend

	ystart=-rcenterlength
	if driftrange < 0.0000001:
		yend=rlongest-rcenterlength
	else:
		yend=driftrange
	print "::Cummulative drift range for plot is ",driftrange,".  Using ",yend
	
	print "::Maximal number of frames found in one movie was ",rmostframes

	x=[n for n in range(rmostframes-1)];
	print "len = ",len(x)," and ",len(rcumlen)

        plt.figure(figsize=(14,14))
        # plt.subplot(111,autoscale_on=False,aspect='equal',xlim=[xstart,xend],ylim=[ystart,yend])
        plt.subplot(111,autoscale_on=False,xlim=[xstart,xend],ylim=[ystart,yend])

	print "::average = ",average


	if average == 0:
		ramp=0.7
		roff=0
	else:
		ramp=0.8
		roff=1.2-ramp
	rpha=0.0

	if average == 0 or average == 2:
		print "::Plotting all ",len(files)," profiles."
		for n in range(0,len(files)):
			y=[];
			for i in range(rmostframes-1):
				y.append(rcumlen[n][i])
			rpha=rpha+360.0/len(files)
			rgbr=ramp*(0.5*np.sin((rpha      )*np.pi/180.0)+0.5)+roff
			rgbg=ramp*(0.5*np.sin((rpha+120.0)*np.pi/180.0)+0.5)+roff
			rgbb=ramp*(0.5*np.sin((rpha+240.0)*np.pi/180.0)+0.5)+roff
			if rgbr > 1.0:
				rgbr = 1.0
			if rgbg > 1.0:
				rgbg = 1.0
			if rgbb > 1.0:
				rgbb = 1.0

			if dots == "y":
				plt.plot(x,y,'bo',markersize=1,linewidth=0.2,color=(rgbr,rgbg,rgbb))
			plt.plot(x,y,markersize=1,linewidth=0.2,color=(rgbr,rgbg,rgbb))
        
	if average == 1 or average == 2:
		print "::Plotting average profile."
		y=[];
		for i in range(rmostframes-1):
			y.append(raver[i])
		if dots == "y":
			plt.plot(x,y,'bo',markersize=2,linewidth=3.0,color=(0.0,0.0,1.0))
		plt.plot(x,y,markersize=2,linewidth=3.0,color=(0.0,0.0,1.0))
        
        plt.title('Drift profile')
        plt.xlabel('Frame Number')
        plt.ylabel('Cummulative Drift [px]')
        plt.grid(True)

        plt.savefig(outfile)

