""" Class for MRC Images

    mostly for reading and writing the Medical Research Council image format
"""

__author__ = "Marcel Arheit (marcel.arheit@unibas.ch)"
__version__ = "$Revision: 1.0 $"
__date__ = "$Date: 24.02.2013 $"
__copyright__ = "Copyright (c) 2013 Marcel Arheit"
__license__ = "GNU GENERAL PUBLIC LICENSE (GPL) Version 2"

import struct
import numpy as np
import os

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
		self.__readHeader(f)
		self.__readImage(f)
	
	def __readHeader(self, f):
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
                                        print("labels:")
					for i in range(10):
						bytes_read = f.read(80)
						if not bytes_read: 
							break
						field = struct.unpack(fmt, bytes_read)[0]
                                                print(str(field))
					        labels.append(field)
					self.__dict__.update({'labels':labels})


	def __readImage(self, f):
	        "check if the mode was read by the header"
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



        def __writeHeader(self, f):
            "writes the header of the mrc file"
            print('############## writing header ###############')
            for label in self.field_labels:
                label_key = label[0]
                label_fmt = label[1]
                label_value = self.__dict__.get(label[0])
                if label_fmt != 'P':
                    label_fmt = '='+label_fmt
                    c_struct = struct.pack(label_fmt, label_value)
                    f.write(c_struct)
                else:
                    no_elements = 0
                    if label_key == 'extra':
                        label_fmt = '=f'
                        no_elements = 25
                        for i in range(no_elements):
                            if i < len(label_value):
                                c_struct = struct.pack(label_fmt, label_value[i]) 
                            else:
                                c_struct = struct.pack(label_fmt, 0) 
                            f.write(c_struct)
                    elif label_key == 'labels':
                        "label_key is now labels"
                        label_fmt = '=80s'
                        no_elements = 10 
                        print(label_key)
                        for i in range(no_elements):
                            if i < len(label_value):
                                line = '{0: <80}'.format(label_value[i]) 
                            else:
                                line = '{0: <80}'.format(' ') 
                            c_struct = struct.pack(label_fmt, line) 
                            f.write(c_struct)
	    
        def __writeImage(self, f):
	        "writes out the image"
                if 'mode' in self.__dict__:
			if self.mode == 0:
				print('MRC image mode: '+str(self.mode))
				fmt = 'int8' 
			elif self.mode == 1:
				print('MRC image mode: '+str(self.mode))
				fmt = 'float16' 
			elif self.mode == 2:
				print('MRC image mode: '+str(self.mode))
				fmt = 'float32'
			else:
				raise Exception("Unsupported MRC image mode: "+self.mode)
			self.image.astype(fmt).tofile(f)
		else:
			sys.exit('Error: The mode in the image header was not read!')

        def __update_Header(self):
            "updates the header parameters according to the image characteristics"
            print(":: UPDATING HEADER")
            dims = np.shape(self.image)
            self.nx = self.mx = dims[1]
            print('nx = '+str(self.nx))
            self.ny = self.my = dims[0]
            if len(dims) > 2:
                self.nz = self.mz = dims[2]
            else:
                self.nz = self.mz = 1
            #mode is kept as before
            self.nxStart = self.nyStart = self.nzStart = 0;
            self.amin = self.image.min() 
            self.amax = self.image.max() 
            self.amean = self.image.mean()
            d = self.image-self.amean
            self.rms = np.sqrt(np.mean(d*d))
            
            
        def setImage(self, image):
            "update the image through the specified img"
            self.image = image
            #self.image = image
            self.__update_Header()

		    
        def tofile(self, filename):
            """writes the mrc image and header to the file with the specified filename"""
            with open(filename,'wb') as f:
                self.__writeHeader(f)
                self.__writeImage(f)

