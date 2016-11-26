import os,sys
import math
from PIL import Image, ImageDraw, ImageFont, ImageFilter

if __name__ == "__main__":

        if len(sys.argv) != 5:
                sys.exit("Usage: PNGannotator.py <Input-Image> <Output-Image> <X,Y,CC-value star file> <cicle diameter>")

        infilename = sys.argv[1]
        outfilename = sys.argv[2]
        starfilename = int(sys.argv[3])
        diameter = int(sys.argv[4])

        image = Image.open(infilename)

        starfile = open(starfilename, 'r')
  
        for i in range (0,8):
                starfile.readline()

        xpos=[]; ypos=[]; ccva=[];

        for line in starfile:
                data_split = line.split()
                xpos.append(float(data_split[0]))
                ypos.append(float(data_split[1]))
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

        for i in range(0,len(xpos)):

                x1 = xpos[i] - diameter / 2.0
                x2 = xpos[i] + diameter / 2.0
                y1 = ypos[i] - diameter / 2.0
                y2 = ypos[i] + diameter / 2.0

                cr = ( (ccva[i]-ccmin) / ccdiff ) * 128 + 128
                cg = ( (ccva[i]-ccmin) / ccdiff ) * 128 + 128
                cb = 0.0
                ct = 255

                PIL.ImageDraw.Draw.ellipse(x1,y1,x2,y2,fill=None,outline=(cr,cg,cb,ct))

        image.save(outfilename)

        starfile.close()

