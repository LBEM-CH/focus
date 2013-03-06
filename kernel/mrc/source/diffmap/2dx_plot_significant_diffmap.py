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
	header = []
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
	plotImage(im1.image, max_val,"map1")
	saveImage(im1)
	plotImage(im2.image, max_val,"map1")
	saveImage(im2)
        contour = im1.image
        diffmap = significantDifferences(mrc_images[0], mrc_images[1], mrc_images[2], mrc_images[3])
        variance = getDiffmap(im_mixed1,im_mixed2)
        raw_diffmap = getDiffmap(mrc_images[0],mrc_images[1])
        plotImage(variance, max_val, "variance")
        plotImage(raw_diffmap, max_val, "diffmap raw")
	if no_args < 7:
	    plotDiffmap(contour, diffmap)
	elif no_args == 7:
	    plotDiffmap(contour, diffmap, sys.argv[5], sys.argv[6])
        else:
	    plotDiffmap(contour, diffmap, sys.argv[5], sys.argv[6], sys.argv[7])
	plt.show()

	

	

	
