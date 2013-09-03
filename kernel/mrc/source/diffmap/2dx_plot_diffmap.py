import sys
import os
from optparse import OptionParser
from plotting import *



if __name__ == '__main__':
    no_args = len(sys.argv)
    usage = 'usage: %prog [options] map1 map2'
    parser = OptionParser(usage=usage)
    parser.add_option("--map1-name",
            default="",
            help="name for the specified first map used in the plot") 
    parser.add_option("--map2-name",  
            default="",
            help="name for the specified first map used in the plot") 
    parser.add_option("-c", "--contour-map",  
            help="map used for zero cotouring, this is need in with the t-Welch's method. Default the conour from map1 is taken")
    parser.add_option("-v", "--varmap",
            help='variation map used to determine the significance of the differences in the diffmap')
    parser.add_option("-f", "--variation-factor", 
            type="float",
            default=1.0, 
            help="factor with which the variation is multiplied when comparing to the raw differences")
    parser.add_option("--plot-range", 
            type="float",
            help="specfies the range that is plotted in the diffmap i.e. [-val, val]")
    parser.add_option("--plot-scalebar", 
            action="store_true", default=False,
            help="plots the map with a scale bar instead of the ticks on the axes")
    parser.add_option("--plot-map-only", 
            action="store_true", default=False,
            help="plots only the difference map without color bar and title etc.")
    parser.add_option("--colormap", 
            default="jet", 
            help="colormap used for coloring the difference map.")
    parser.add_option("-s", "--shift180",
            action="store_true", default=False,
            help='shifts the diffmap half a unit cell in x direction, default is jet')
    (options, args) = parser.parse_args()
    no_args = len(args)
    if no_args < 2:
        parser.error("you have to specify both maps as arguments")
    diffmap1_filepath = args[0]
    diffmap2_filepath = args[1]
    if options.contour_map:
        map1_filepath = options.contour_map
    else:
        map1_filepath = diffmap1_filepath

            
    with open(map1_filepath,'r') as mrcFile:
             im1 = MRCImage(mrcFile)	
    with open(diffmap1_filepath,'r') as mrcFile:
             im1sig = MRCImage(mrcFile)	
    with open(diffmap2_filepath,'r') as mrcFile:
             im2sig = MRCImage(mrcFile)
    [width, height] = cropImages(im1sig,im2sig)
    cropImage(im1, width, height)
    images = [im1sig, im2sig]
    max_val = scaleImages(images)
    cutImages(images)
    images.append(im1)
    if options.shift180:
        images = shiftImagesHalfX(images)
    plotImage(images[0].image, 1.0, options.map1_name)
    saveImage(images[0])
    plotImage(images[1].image, 1.0, options.map2_name)
    saveImage(images[1])
    contour = images[2].image
    
    #TODO: scale amplitudes
    plotImage(contour, 1.0, "contour")
    #plotImage(images[0].image -contour, 1.0, "diffmap contour")

    plot_options = {}
    if options.map1_name and options.map2_name:
        plot_options['map1_name'] = options.map1_name
        plot_options['map2_name'] = options.map2_name
    plot_options['colormap'] = options.colormap
    if options.plot_range: 
        plot_options['max_range'] = options.plot_range
    if options.plot_scalebar:
        plot_options['plot_scalebar'] = True
    if options.plot_map_only:
        plot_options['map_only'] = True
    if options.varmap:
        #TODO: check if varmap is scaled correctly
        varmap = getImage(options.varmap)
        raw_diffmap = getDiffmap(images[0],images[1])
        plotImage(raw_diffmap, 0.0, "raw difference map")
        diffmap = significantDifferences(raw_diffmap, varmap, options.variation_factor)
        #plotImage(varmap, 0.0, "variation")
        plotVarmap(contour,varmap,plot_options)
    else:
        diffmap = getDiffmap(images[0],images[1])

    plotDiffmap(contour, diffmap, plot_options)
    plt.show()

	

	

	
