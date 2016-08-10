from EMAN2 import *
from sparx import *

import os
import sys


if __name__ == "__main__":
	
	if len(sys.argv) != 3:
		sys.exit("Missuse detected")

	dirlist = sys.argv[1]
	print ":Evaluating directories listed in ", dirlist
	
	outfile = sys.argv[2]
	print ":Output will be placed into ", outfile
	
        directories = open(dirlist, 'r')

        defmax = -1e10
        defmin =  1e10
        TANGLmax = -1e10

        for dirfolder in directories:
                configfile = "../" + dirfolder.strip() + "/2dx_image.cfg"
                if os.path.isfile(configfile):
                        f = open(configfile, 'r')
                        for l in f:
                                if l.startswith("set defocus ="):
                                        defocus = l.split('"')[1]
                                        def1 = float(defocus.split(',')[0])
                                        def2 = float(defocus.split(',')[1])
                                        defave = ( def1 + def2 ) / 2.0
                                        print "File ", configfile, "has defocus of ", def1, ", ", def2, ", => average = ",defave
                                        if defmax < defave:
                                                defmax = defave
                                        if defmin > defave:
                                                defmin = defave
        
                                if l.startswith("set TANGL ="):
                                        TANGL = float(l.split('"')[1])
                                        if TANGLmax < TANGL:
                                                TANGLmax = TANGL
                        print "File ", configfile, "has defocus of ", def1, ", ", def2, ", => average = ",defave, " and TANGL = ", TANGL
        
        print ":Maximum defocus found is ", defmax
        print ":Minimum defocus found is ", defmin
        print ":Maximum TANGL   found is ", TANGLmax

        lines = []
        lines.append(str(defmin) + ' ' + str(defmax) + ' ' + str(TANGLmax) + '\n')

        out_file = open(outfile, "w")
        out_file.writelines(lines)
        out_file.close()
               
