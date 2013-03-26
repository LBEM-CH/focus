import sys
import os
from plotting import *



if __name__ == '__main__':
	no_args = len(sys.argv)
	if no_args < 5:
		sys.exit('Usage: python '+sys.argv[0]+' <map1> <map2> <map_mixed1> <map_mixed2>')
	map1_filepath=sys.argv[1]
	map2_filepath=sys.argv[2]
	map_mixed1_filepath=sys.argv[3]
	map_mixed2_filepath=sys.argv[4]
        if no_args >= 7:
            map1_name = sys.argv[5]
            print("::map 1 name: "+map1_name)
            map2_name = sys.argv[6]
            print("::map 2 name: "+map2_name)
        else:
            # this is to allow plotting without title
            map1_name = ""
            map2_name = ""
        if no_args > 7:
            colormap = sys.argv[7]
        else:
            colormap = "jet"
	shift = False
        if no_args >= 9:
            shift = int(sys.argv[8])
	with open(map1_filepath,'r') as mrcFile:
		 im1 = MRCImage(mrcFile)	
	with open(map2_filepath,'r') as mrcFile:
		 im2 = MRCImage(mrcFile)	
	with open(map_mixed1_filepath,'r') as mrcFile:
		 im_mixed1 = MRCImage(mrcFile)
	with open(map_mixed2_filepath,'r') as mrcFile:
		 im_mixed2 = MRCImage(mrcFile)
        [width, height] = cropImages(im1,im2)
        cropImage(im_mixed1, width, height)
        cropImage(im_mixed2, width, height)
        mrc_images = [im1,im2,im_mixed1,im_mixed2]
	max_val = scaleImages(mrc_images)
	cutImages(mrc_images)
        if shift == True:
            print "shifting images half a unit cell size in x"
            mrc_images = shiftImagesHalfX(mrc_images)
	plotImage(mrc_images[0].image, max_val, map1_name)
	saveImage(mrc_images[0])
	plotImage(mrc_images[1].image, max_val, map2_name)
	saveImage(mrc_images[1])
        contour = mrc_images[0].image
        diffmap = significantDifferences(mrc_images[0], mrc_images[1], mrc_images[2], mrc_images[3])
        variance = getDiffmap(mrc_images[2],mrc_images[3])
        raw_diffmap = getDiffmap(mrc_images[0],mrc_images[1])
        plotImage(variance, 0.0, "variance")
        plotImage(raw_diffmap, 0.0, "diffmap raw")
	    
        plotDiffmap(contour, diffmap, map1_name, map2_name, colormap)
	plt.show()

	

	

	
