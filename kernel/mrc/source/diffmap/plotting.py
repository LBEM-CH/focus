import numpy as np
import matplotlib.pyplot as plt
import matplotlib.colors as colors
from matplotlib.ticker import MultipleLocator
from mpl_toolkits.axes_grid1.anchored_artists import AnchoredSizeBar
from mrcimage import *
                

def getImage(mrcfilepath):
    """reads the mrc image and returns the image as numpy array"""
    with open(mrcfilepath,'r') as mrcFile:
        mrc = MRCImage(mrcFile)
        return  mrc.image 

def scaleImages(mrc_image_list):
        m = max([ np.absolute(mrcImage.image).max() for mrcImage in mrc_image_list])
	print('Max Intensity in the images is: '+str(m))
        for mrcImage in mrc_image_list:
            mrcImage.image = mrcImage.image/m
	return m 

def cropImage(mrcImage, width, height):
    "crops the mrc image to the width and height"
    image = mrcImage.image
    [orig_height, orig_width] = np.shape(image)
    if width == orig_width and height == orig_height:
        return mrcImage
    elif width > orig_width and height > orig_height:
        exit("the image cannnot be cropped to an size larger then itself")
    else:
        d_width = orig_width - width
        d_height = orig_height - height
        d2_width = d_width/2
        d2_height = d_height/2
        if d2_width > 0:
            if d_width % 2 > 0:
                istart = d2_width
            else:
                istart = d2_width-1
            iend = -d2_width-1
            image = image[:,istart:iend]
        if d2_height > 0:
            if d_height % 2 > 0:
                istart = d2_height
            else:
                istart = d2_height-1
            iend = -d2_height-1
            image = image[istart:iend,:]
        mrcImage.image = image
        mrcImage.nx = width
        mrcImage.ny = height
        return mrcImage





def cropImages(mrcImage1, mrcImage2):
        """if the images do not habve the same size they are cropped to the smaller
        width and height"""
        width1 = mrcImage1.nx
        height1 = mrcImage1.ny
        width2 = mrcImage2.nx
        height2 = mrcImage2.ny
        print(": image 1 size:"+str(np.shape(mrcImage1.image)))
        print(": image 2 size:"+str(np.shape(mrcImage2.image)))
        if width1 == width2 and height1 == height2:
            #no cropping needed
            return (width1, height1)
        [width_min, width_max] = minmax(width1, width2) 
        [height_min, height_max] = minmax(height1, height2)
        d_width = width_max - width_min
        d2_width = d_width/2
        d_height = height_max - height_min
        d2_height = d_height/2
        if d2_width > 0:
            if d_width % 2 > 0:
                istart = d2_width-1
            else:
                istart = d2_width
            iend = -d2_width
            print(": width difference: "+str(d_width)+" start: "+str(istart)+" end: "+str(iend))
            if width1 > width_min:
                mrcImage1.image = mrcImage1.image[:,istart:iend]
                #mrcImage1.image = mrcImage1.image[istart:iend,:]
                mrcImage1.nx = width_min
            else:
                mrcImage2.image = mrcImage2.image[:,istart:iend]
                mrcImage2.nx = width_min
        if d2_height > 0:
            if d_height % 2 > 0:
                istart = d2_height-1
            else:
                istart = d2_height
            iend = -d2_height
            print(": height difference: "+str(d_height)+" start: "+str(istart)+" end: "+str(iend))
            if height1 > height_min:
                mrcImage1.image = mrcImage1.image[istart:iend,:]
                mrcImage1.ny = height_min
            else:
                mrcImage2.image = mrcImage2.image[istart:iend,:]
                mrcImage2.ny = height_min
        print(": image 1 cropped to size:"+str(np.shape(mrcImage1.image)))
        print(": image 2 cropped to size:"+str(np.shape(mrcImage2.image)))
        return (width_min, height_min)

def minmax(value1, value2):
    """compares the values and returns the minimum and maximum"""
    if value1 < value2:
        return [value1, value2]
    else:
        return [value2, value1]

def cutImages(mrc_image_list):
    for mrc_image in mrc_image_list:
        im = mrc_image.image
        im[im<0] = 0.0
	mrc_image.image = im

def shiftImage(image, xshift, yshift):
    """shifts the image by x shift and y shift"""
    im_xshift = np.roll(image, xshift,axis=1)
    im_xyshift = np.roll(im_xshift, yshift,axis=0)
    return im_xyshift


def shiftImages(mrc_image_list, xshift, yshift):
    """shifts the images in the list by x shift and y shift"""
    for mrc_image in mrc_image_list:
        im = shiftImage(mrc_image.image, xshift, yshift)
	mrc_image.image = im
    return mrc_image_list

def shiftHalfUnitCellX(image):
    width = np.size(image,1)
    half_x = width/2
    im_shifted = shiftImage(image, half_x, 0)
    return im_shifted

def shiftImagesHalfX(mrc_image_list):
    """shifts all images in the list half a unit cell in x direction"""
    for mrc_image in mrc_image_list:
        im = shiftHalfUnitCellX(mrc_image.image)
	mrc_image.image = im
    return mrc_image_list



def saveImage(mrcImage):
	image = mrcImage.image
	title = mrcImage.name
	filename = title+'.pdf'
	plt.imsave(filename, image,  vmin=None, vmax=None, cmap=plt.cm.jet, format='pdf', origin='lower')
 	return image

def getDiffmap(mrc_image1, mrc_image2):
    """returns the difference map of the two images"""
    im1 = mrc_image1.image  
    im2 = mrc_image2.image
    diffmap = im1-im2
    return diffmap

def getAbsDiffmap(image1, image2):
    "return the absolute of the differenece of image1 and image2"
    diffmap = image1 - image2
    return np.abs(diffmap)

def significantDifferences(diffmap, varmap, var_factor=1.0):
    """determines which differences are significant based on the variation map """
    diffmap[abs(diffmap)<var_factor*abs(varmap)] = 0.0
    return diffmap

def addScaleBar(axes):
    "adds a scale bar to the figure and removes the ticks"
    axes.get_xaxis().set_visible(False)
    axes.get_yaxis().set_visible(False)
    bar = AnchoredSizeBar(axes.transData, 20, '10 $\AA$',loc=4, sep=5, frameon=False)
    axes.add_artist(bar)

def scaleTicks(axes):
    "the ticks have to be scaled by a factor of 2, since we plot CCP4 2 * (unit cell size - unti cell size % 4)"
    xticks = axes.get_xticks()/2
    yticks = axes.get_yticks()/2
    axes.set_xticklabels(xticks)
    axes.set_yticklabels(yticks)
    axes.set_xlabel('$\AA$')

def plotImage(image, crange=0.0, title="", plot_scalebar=False):
        plt.figure()
	plt.title(title)
        if crange > 0.0:
	    norm = colors.Normalize(vmin=0.0, vmax=crange) 
	    plt.imshow(image, norm=norm)
        else:
	    plt.imshow(image)
        ax = plt.axes()
        if plot_scalebar:
            addScaleBar(ax)
        else:
            scaleTicks(ax)
        plt.colorbar()
        if title:
	    filename = title+'.pdf'
            if os.path.isdir("diffmap"):
                filename = os.path.join("diffmap", filename)
	    plt.savefig(filename)


def plotMRCImage(mrcImage,crange=0.0):
	image = mrcImage.image
	plotImage(image, crange, mrcImage.name) 	
        return image

def plotContouredMap(contour, diffmap, title, options):
        if 'max_range' in options:
            max_range = options['max_range']
        else:
            max_range = max(diffmap.max(),abs(diffmap.min()))
	norm = colors.Normalize(vmin=-max_range, vmax=max_range) 
	print('max_range is '+str(max_range))
        if 'plot_scalebar' in options:
            plot_scalebar = options['plot_scalebar']
        else:
            plot_scalebar = False
        if 'map_only' in options:
            map_only = options['map_only']
        else:
            map_only = False

	fig = plt.figure()
        if title and not map_only:
	    plt.title(title)
        plt.hold(True)
	plt.imshow(diffmap, origin='upper', norm=norm)
        ax = plt.axes()
        if plot_scalebar:
            addScaleBar(ax)
        else:
            scaleTicks(ax)
        if 'colormap' in options:
            plt.set_cmap(options['colormap'])
        if not map_only:
            plt.colorbar()
	plt.contour(contour, [0], colors='b')
	plt.hold(False)
        filename = title+'.pdf'
        filename.replace(' ', '_') 
        if os.path.isdir("diffmap"):
            filename = os.path.join("diffmap", filename)
        if map_only:
	    plt.savefig(filename, bbox_inches='tight', pad_inches=0)
        else:
	    plt.savefig(filename)
        plt.show()
	return diffmap

def plotDiffmap(contour, diffmap, options):
        if 'map1_name' in options and 'map2_name' in options:
            title = options['map1_name']+' - '+options['map2_name']
	    filename = title+'.pdf'
        else:
            title = "diffmap"
        plotContouredMap(contour, diffmap, title, options)
       	return diffmap

def plotVarmap(contour, varmap, options):
        if 'map1_name' in options and 'map2_name' in options:
            title = 'varmap '+options['map1_name']+' & '+options['map2_name']
        else:
            title = "varmap"
        varmap = plotContouredMap(contour, varmap, title, options)
       	return varmap 


def plotTwoContours(contour, contour2, diffmap, mapName1="map1", mapName2="map2", colormap="jet", plot_scalebar=False):
        #DEBUG:
        #max_range = 0.07
        max_range = max(diffmap.max(),abs(diffmap.min()))
	norm = colors.Normalize(vmin=-max_range, vmax=max_range) 
	print('max_range is '+str(max_range))
	title = mapName1+' - '+mapName2
	filename = title+'.pdf'
	fig = plt.figure()
	plt.title(title)
        plt.hold(True)
	plt.imshow(diffmap, origin='upper', norm=norm)
        ax = plt.axes()
        if plot_scalebar:
            addScaleBar(ax)
        else:
            scaleTicks(ax)
        #if(colormap == "rwb")
        plt.set_cmap(colormap)
	plt.colorbar()
	plt.contour(contour, [0], colors='r')
	#plt.contour(image2, [0], colors='b')
	plt.contour(contour2, [0], colors='b')
	plt.hold(False)
	plt.savefig(filename)
        plt.show()
	return diffmap

def plotMontage(varmap, diffmap, raw_diffmap, patch_size):
    """plots sub patches of the significant diffmap, the raw diffmap, and the variation map next to each other in a montage"""
    (height,width) = np.shape(diffmap)
    print np.shape(varmap) 
    restx = patch_size - width%patch_size
    resty = patch_size - height%patch_size
    print restx,resty
    xindices = range(0, width, patch_size)
    yindices = range(0, height, patch_size)
    montage =  np.zeros((height+len(yindices)+resty, 3*(width+len(xindices)+restx)))
    varmap_ext = np.zeros((height+resty, width+restx))
    varmap_ext[:height,:width] = varmap
    raw_diffmap_ext = np.zeros((height+resty, width+restx))
    raw_diffmap_ext[:height,:width] = diffmap
    diffmap_ext = np.zeros((height+resty, width+restx))
    diffmap_ext[:height,:width] = diffmap
    j = 0
    for y in yindices:
        i = 0
        for x in xindices:
            montage[j:j+patch_size,i:i+patch_size] = varmap_ext[y:y+patch_size,x:x+patch_size]
            i = i+patch_size
            montage[j:j+patch_size,i:i+patch_size] = diffmap_ext[y:y+patch_size,x:x+patch_size]
            i = i+patch_size
            montage[j:j+patch_size,i:i+patch_size] = raw_diffmap_ext[y:y+patch_size,x:x+patch_size]
            i = i+patch_size
        j = j+patch_size
    #print np.shape(montage)
    #montage = montage[:height+len(yindices),:3*(width+len(xindices))]
    #print np.shape(montage)

    fig = plt.figure()
    ax = fig.add_subplot(111)

    (mheight,mwidth) = np.shape(montage)
    minor_locator = MultipleLocator(patch_size)
    plt.xticks(np.arange(0,mwidth,3*patch_size))
    plt.axes().xaxis.set_minor_locator(minor_locator) 
    plt.yticks(np.arange(0,mwidth,patch_size))
    plt.grid(b=True, which='minor', color='b', linestyle='-')
    plt.grid(b=True, which='major', linestyle='-', linewidth=1.5 )
    plt.imshow(montage)
    cid = fig.canvas.mpl_connect('button_press_event', onclick) 
    cid = fig.canvas.mpl_connect('button_release_event', onclick) 
    plt.show()

def onclick(event):
    """callback function that is fired when the mouse is clicked"""
    print 'button=%d, x=%d, y=%d, xdata=%f, ydata=%f'%(event.button, event.x, event.y, event.xdata, event.ydata)



	
