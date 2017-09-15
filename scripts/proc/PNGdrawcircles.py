import os,sys
import math
from PIL import Image, ImageDraw, ImageFont, ImageFilter

if __name__ == "__main__":

        if len(sys.argv) != 6:
                sys.exit("Usage: PNGannotator.py <Input-Image> <Output-Image> <X,Y,CC-value star file> <cicle diameter> <xoffset>")

        infilename = sys.argv[1]
        outfilename = sys.argv[2]
        starfilename = sys.argv[3]
        diameter = float(sys.argv[4])
	xoffset = float(sys.argv[5])

        image = Image.open(infilename)

	width,height = image.size

        starfile = open(starfilename, 'r')
  
        for i in range (0,9):
                starfile.readline()

        xpos=[]; ypos=[]; ccva=[];

        for line in starfile:
                data_split = line.split()
                xpos.append(        float(data_split[0]) - xoffset )
                ypos.append((height-float(data_split[1])))
                ccva.append(float(data_split[4]))

        ccmax = 0.0
        ccmin = 100000.0
        for i in range(0,len(xpos)):
                if ccva[i] < ccmin:
                        ccmin = ccva[i]
                if ccva[i] > ccmax:
                        ccmax = ccva[i]

        ccdiff = ccmax - ccmin
        if ccdiff < 0.01:
                ccdiff = 0.01

        print "Found ",len(xpos)," particle positions."
        print "CCvalues range from ",ccmin," to ",ccmax,"."

	draw = ImageDraw.Draw(image)

        for i in range(0,len(xpos)):

                cr = int(( (ccva[i]-ccmin) / ccdiff ) * 128 + 128)
                cg = int(( (ccva[i]-ccmin) / ccdiff ) * 128 + 128)
                cb = int(128)
                ct = int(255)

		for n in range (0,20):
	                x1 = xpos[i] - (diameter / 2.0 - n/2.0)
   	             	x2 = xpos[i] + (diameter / 2.0 - n/2.0)
  	              	y1 = ypos[i] - (diameter / 2.0 - n/2.0)
  	              	y2 = ypos[i] + (diameter / 2.0 - n/2.0)
                	draw.ellipse((x1,y1,x2,y2),fill=None,outline=(cr,cg,cb,ct))
        
	del draw

	image.save(outfilename)

        starfile.close()

