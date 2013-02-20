import sys
import os
import struct
import array 
import numpy as np
import scipy.misc
import matplotlib.pyplot as plt
import matplotlib.colors as colors
import matplotlib.pylab as plab

class MRCImage:

	field_labels =  (('nx', 'i'), 
			('ny','i'), 
			('nz','i'), 
			('mode','i'), 
			('nxStart','i'), 
			('nyStart','i'), 
			('nzStart','i'), 
			('mx','i'), 
			('my','i'), 
			('mz','i'), 
			('a','f'), 
			('b','f'), 
			('c','f'),
			('alpha','f'),
			('beta','f'),
			('gamma','f'),
			('mapc','i'),
			('mapr','i'),
			('maps','i'),
			('amin','f'),
			('amax','f'),
			('amean','f'),
			('ispg','i'),
			('nsymbt','i'),
			('extra','P'),
			('xOrigin','f'),
			('yOrigin','f'),
			('zOrigin','f'),
			('map','4s'),
			('machst','4s'),
			('rms','f'),
			('nlabl','i'),
			('labels','P'),
			)

	def __init__(self, f):
		filename = os.path.basename(f.name)
		self.name = os.path.splitext(filename)[0]
		self.readHeader(f)
		self.readImage(f)
	
	def readHeader(self, f):
		for label in self.field_labels:
			fmt = '='+label[1]
			if label[1] != 'P':
				bytes_read = f.read(4)
				if not bytes_read: 
					break
				field = struct.unpack(fmt, bytes_read)[0]
				print(label[0]+' = '+str(field))
				self.__dict__.update({label[0]:field})
			else:
				if label[0] == 'extra':
					extra = []
					fmt = '=f'
					for i in range(25):
						bytes_read = f.read(4)
						if not bytes_read: 
							break
						field = struct.unpack(fmt, bytes_read)[0]
						extra.append(field)
					self.__dict__.update({'extra':extra})
				elif label[0] == 'labels':
					labels = []
					fmt = '80s'
					for i in range(10):
						bytes_read = f.read(80)
						if not bytes_read: 
							break
						field = struct.unpack(fmt, bytes_read)[0]
						labels.append(field)
					self.__dict__.update({'lalbels':labels})
	
	def readImage(self, f):
		# check if the mode was read by the header
		if 'mode' in self.__dict__:
			if self.mode == 0:
				print('MRC image mode: '+str(self.mode))
				fmt = np.uint8
				#self.image = array.array('b')
			elif self.mode == 1:
				print('MRC image mode: '+str(self.mode))
				fmt = np.float16
			elif self.mode == 2:
				print('MRC image mode: '+str(self.mode))
				fmt = np.float32
				#self.image = array.array('f')
				#self.image.fromfile(f,self.nx*self.ny*self.nz)
			else:
				raise Exception("Unsupported MRC image mode: "+self.mode)
			
			self.image = np.fromfile(f,fmt,self.nx*self.ny*self.nz)
			if(self.nz == 1):
				self.image = self.image.reshape(self.ny,self.nx)
			else:
				self.image = self.image.reshape(self.ny, self.nx, self.nz)
				print('Warning: Image dimensions: '+str(self.image.shape))
		else:
			sys.exit('Error: The mode in the image header was not read!')
	
def scaleImages(mrcImage1, mrcImage2):
	image1 = mrcImage1.image
	image2 = mrcImage2.image
	m = max((np.absolute(image1)).max(),(np.absolute(image2)).max())
	print('Max Intensity in the images is: '+str(m))
	mrcImage1.image = image1/m
	mrcImage2.image = image2/m
	return m 

def cropImages(mrcImage1, mrcImage2):
        """if the images do not habve the same size they are cropped to the smaller
        width and height"""
        width1 = mrcImage1.nx
        height1 = mrcImage1.ny
        width2 = mrcImage2.nx
        height2 = mrcImage2.ny
        if width1 == width2 and height1 == height2:
            #no cropping needed
            return [mrcImage1, mrcImage2]
        [width_min, width_max] = minmax(width1, width2) 
        [height_min, height_max] = minmax(height1, height2)
        d_width = width_max - width_min
        d2_width = d_width/2
        d_height = height_max - height_min
        d2_height = d_height/2
        if d2_width > 0:
            if d2_width % 2 > 0:
                istart = d2_width
            else:
                istart = d2_width-1
            iend = -d2_width-1
            if width1 > width_min:
                mrcImage1.image = mrcImage1.image[:,istart:iend]
                #mrcImage1.image = mrcImage1.image[istart:iend,:]
                mrcImage1.nx = width_min
            else:
                mrcImage2.image = mrcImage2.image[:,istart:iend]
                mrcImage2.nx = width_min
        if d2_height > 0:
            if d2_height % 2 > 0:
                istart = d2_height
            else:
                istart = d2_height-1
            iend = -d2_height-1
            if height1 > height_min:
                mrcImage1.image = mrcImage1.image[istart:iend,:]
                mrcImage1.ny = height_min
            else:
                mrcImage2.image = mrcImage2.image[istart:iend,:]
                mrcImage2.ny = height_min
        print(": image 1 cropped to size:"+str(np.shape(mrcImage1.image)))
        print(": image 2 cropped to size:"+str(np.shape(mrcImage2.image)))
        return [mrcImage1,mrcImage2]

def minmax(value1, value2):
    """compares the values and returns the minimum and maximum"""
    if value1 < value2:
        return [value1, value2]
    else:
        return [value2, value1]

def cutImages(mrcImage1, mrcImage2):
	im1 = mrcImage1.image
	im2 = mrcImage2.image
	im1[im1<0] = 0
	im2[im2<0] = 0
	mrcImage1.image = im1
	mrcImage2.image = im2

def saveImage(mrcImage):
	image = mrcImage.image
	title = mrcImage.name
	filename = title+'.pdf'
	plt.imsave(filename, image,  vmin=None, vmax=None, cmap=plt.cm.jet, format='pdf', origin='lower')
 	return image



def plotImage(mrcImage):
	image = mrcImage.image
	fg =  plt.figure()
	title = mrcImage.name
	plt.title(title)
	plt.imshow(image)
	return image


def plotDiffmap(mrcImage1, mrcDiff1, mrcDiff2, mapName1="map1", mapName2="map2"):
	contour = mrcImage1.image
	image1 = mrcDiff1.image
	image2 = mrcDiff2.image
	diffmap = image1-image2
	max_range = max(diffmap.max(),abs(diffmap.min()))
	norm = colors.Normalize(vmin=-max_range, vmax=max_range) 
	print('max_range is '+str(max_range))
	title = mapName1+' - '+mapName2
	filename = title+'.pdf'
	fig = plt.figure()
	plt.title(title)
	plt.hold(True)
	plt.imshow(diffmap, origin='upper', norm=norm)
        plt.set_cmap('seismic')
	plt.colorbar()
	plt.contour(contour, [0])
	plt.hold(False)
	plt.savefig(filename)
	return diffmap




if __name__ == '__main__':
	no_args = len(sys.argv)
	if no_args < 4:
		sys.exit('Usage: python '+sys.argv[0]+' <map1> <map1-sig> <map2-sig>')
	map1_filepath=sys.argv[1]
	diffmap1_filepath=sys.argv[2]
	diffmap2_filepath=sys.argv[3]
	header = []
	with open(map1_filepath,'r') as mrcFile:
		 im1 = MRCImage(mrcFile)	
	with open(diffmap1_filepath,'r') as mrcFile:
		 im1sig = MRCImage(mrcFile)	
	with open(diffmap2_filepath,'r') as mrcFile:
		 im2sig = MRCImage(mrcFile)
        cropImages(im1sig,im2sig)
	scaleImages(im1sig,im2sig)
	cutImages(im1sig,im2sig)
	plotImage(im1sig)
	saveImage(im1sig)
	plotImage(im2sig)
	saveImage(im2sig)
	if no_args < 6:
		plotDiffmap(im1,im1sig,im2sig)
	else:
		plotDiffmap(im1,im1sig,im2sig, sys.argv[4], sys.argv[5])
	plt.show()

	

	

	
