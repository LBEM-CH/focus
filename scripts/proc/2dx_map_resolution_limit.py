from EMAN2 import *
from sparx import *

import os


if __name__ == "__main__":
        
        if len(sys.argv) != 5:
                sys.exit("Missuse detected")

        input_map = sys.argv[1]
        output_map = sys.argv[2]
        resolution = sys.argv[3]
        apix = sys.argv[4]
        
        inimage = get_image(input_map)
        nx = inimage.get_xsize()
        ny = inimage.get_ysize()
        nz = inimage.get_zsize()
        
        print ( ":Input map = ", input_map )
        print ( ":nx,ny,nz =", nx,ny,nz )
        print ( ":resolution limit = ", resolution )
        print ( ":Angstroem per pixel = ", apix )
        absresol = float(apix) / float(resolution)
        
        print ( ":Abolute resolution limit = ", absresol )
        outimage = filt_tophatl(inimage,absresol,pad=True)

        outimage.write_image(output_map)

