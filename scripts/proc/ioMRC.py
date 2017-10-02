from __future__ import division, print_function, absolute_import, unicode_literals
"""
Conventional MRC2014 and on-the-fly compressed MRCZ file interface

CCPEM MRC2014 specification:

http://www.ccpem.ac.uk/mrc_format/mrc2014.php

IMOD specification:

http://bio3d.colorado.edu/imod/doc/mrc_format.txt

Testing:

http://emportal.nysbc.org/mrc2014/

Tested output on: Gatan GMS, IMOD, Chimera, Relion, MotionCorr, UnBlur
"""

# Modified on Thu Feb 09 2015 by Ricardo Righetto

import os, os.path
import numpy as np
import threading
import struct
from concurrent.futures import ThreadPoolExecutor
import logging
logger = logging.getLogger('MRCZ')
try: 
    import blosc
    bloscPresent = True
    # For async operations we want to release the GIL in blosc operations and 
    # file IO operations.
    blosc.set_releasegil(True)
except:
    bloscPresent = False
    logger.info( "`blosc` meta-compression library not found, file compression disabled." )
try: 
    import rapidjson as json
except ImportError:
    import json
    logger.info( "`python-rapidjson` not found, using builtin `json` instead." )

# Buffer for file I?O
# Quite arbitrary, in bytes (hand-optimized)
BUFFERSIZE = 2**20
BLOSC_BLOCK = 2**16

DEFAULT_HEADER_LEN = 1024

# ENUM dicts for our various Python to MRC constant conversions
COMPRESSOR_ENUM = { 0:None, 1:'blosclz', 2:'lz4', 3:'lz4hc', 4:'snappy', 5:'zlib', 6:'zstd' }
REVERSE_COMPRESSOR_ENUM = { None:0, 'blosclz':1, 'lz4':2, 'lz4hc':2, 'snappy':4, 'zlib':5, 'zstd':6 }

MRC_COMP_RATIO = 1000  
CCPEM_ENUM = { 0: 'i1', 1:'i2', 2:'f4', 4:'c8', 6:'u2', 101:'u1' }
EMAN2_ENUM = { 1: 'i1', 2:'u1', 3:'i2', 4:'u2', 5:'i4', 6:'u4', 7:'f4' }
REVERSE_CCPEM_ENUM = { 'int8':0, 'i1':0, 
                      'uint4':101, 
                      'int16':1, 'i2':1,
                      'uint16':6, 'u2':6,
                      'float64':2, 'f8':2, 'float32':2, 'f4':2,
                      'complex128':4, 'c16':4, 'complex64':4, 'c8':4 }


_asyncExecutor = ThreadPoolExecutor( max_workers = 1 )
def setAsyncWorkers( N_workers ):
    """
    setAsyncWorkers( N_workers )

    Sets the maximum number of background workers that can be used for reading 
    or writing with the functions.  Defaults to 1. Generally when writing 
    to hard drives use 1 worker.  For SSDs and parallel file systems (PFS) 
    network drives more workers can accelerate file IO rates.
    
    Some test results, 30 files, each 10 x 2048 x 2048 x float-32, on a CPU
    with 4 physical cores:

        HD, 1 worker:   42.0 s
        HD, 2 workers:  50.0 s
        HD, 4 workers:  50.4 s
        SSD, 1 worker:  12.6 s
        SSD, 2 workers: 11.6 s
        SSD, 4 workers: 8.9 s
        SSD, 8 workers: 16.9 s

    We observed that caches halted for periods ~5-10 s when too many workers 
    were used.
    """
    if N_workers <= 0:
        raise ValueError("N_workers must be greater than 0")
    if _asyncExecutor._max_workers == N_workers:
        return
    _asyncExecutor._max_workers = N_workers
    _asyncExecutor._adjust_thread_count()

def defaultHeader():
    """
    Returns a default MRC header dictionary with all fields with default values.
    """
    header = {}
    header['fileConvention'] = "ccpem"
    header['endian'] = 'le'
    header['MRCtype'] = 0
    header['dimensions'] = np.array( [0,0,0], dtype=int )
    header['dtype'] = 'u1'
    
    header['compressor'] = None
    header['packedBytes'] = 0
    header['clevel'] = 1
    
    header['maxImage'] = 1.0
    header['minImage'] = 0.0
    header['meanImage'] = 0.0
    
    header['pixelsize'] = 0.1 
    header['pixelunits'] = u"nm" # Can be "\\AA" for Angstroms
    header['voltage'] = 300.0 # kV
    header['C3'] = 2.7 # mm
    header['gain'] = 1.0 # counts/electron
    
    if bloscPresent:
        header['n_threads'] = blosc.detect_number_of_cores()
    
    return header
    
    

def readMRC( MRCfilename, idx=None, endian='le', 
              pixelunits=u'\\AA', fileConvention = "ccpem", 
              useMemmap=False, n_threads = None  ):
    """
    readMRC
    
    Created on Thu Apr 09 11:05:07 2015
    @author: Robert A. McLeod
    
    Imports an MRC/Z file as a NumPy array and a meta-data dict.  
    
    Usage:
        
    [image, meta] = readMRC( MRCfilename, idx=None, endian='le', 
              pixelunits=u'\\AA', fileConvention = "ccpem", 
              useMemmap=False, n_threads = None  ):
    
    * image is a NumPy array.
        
    * meta is a dict with various fields relating to the MRC header information 
      and extended metadata.

    * idx = (first, last) is a tuple with first (inclusive) and last (not 
      inclusive) indices of images to be read from the stack. Index of first image 
      is 0. Negative indices can be used to count backwards. A singleton integer 
      can be provided to read only one image. If omitted, will read whole file. 
      Compression is currently not supported with this option.
    
    * pixelunits can be '\\AA' (Angstoms), 'nm', '\mum', or 'pm'.  Internally pixel 
      sizes are always encoded in Angstroms in the MRC file.
        
    * fileConvention can be 'ccpem' (equivalent to IMOD) or 'eman2', which is
      only partially supported at present.
    
    * endian can be big-endian as 'be' or little-endian as 'le'
        
    * n_threads is the number of threads to use for decompression, defaults to 
       use all virtual cores.

    * useMemmap=True returns a `numpy.memmap` instead of a `numpy.ndarray`

    """

    with open( MRCfilename, 'rb', buffering=BUFFERSIZE ) as f:
        # Read in header as a dict
        
        header = readMRCHeader( MRCfilename, endian=endian, fileConvention = fileConvention, pixelunits=pixelunits )
        # Support for compressed data in MRCZ

        
        if ( (header['compressor'] in REVERSE_COMPRESSOR_ENUM) 
            and (REVERSE_COMPRESSOR_ENUM[header['compressor']] > 0) 
            and idx == None ):
            return __MRCZImport( f, header, endian=endian, fileConvention = fileConvention, 
                                n_threads=n_threads )
        # Else save as MRC file

        if idx != None:
        # If specific images were requested:
        # TO DO: add support to read all images within a range at once

            if header['compressor'] != None:
                raise RuntimeError( "Reading from arbitrary positions not supported for compressed files. Compressor = %s" % header['compressor'] )
            if np.isscalar( idx ):
                indices = np.array( [idx, idx], dtype='int' )
            else:
                indices = np.array( idx, dtype='int' )

            # Convert to old way:
            idx = indices[0]
            n = indices[1] - indices[0] + 1

            if idx < 0:
                # Convert negative index to equivalent positive index:
                idx = header['dimensions'][0] + idx

            # Just check if the desired image is within the stack range:
            if idx < 0 or idx >= header['dimensions'][0]:
                raise ValueError( "Error: image or slice index out of range. idx = %d, z_dimension = %d" % (idx, header['dimensions'][0]) )
            elif idx + n > header['dimensions'][0]:
            	raise ValueError( "Error: image or slice index out of range. idx + n = %d, z_dimension = %d" % (idx + n, header['dimensions'][0]) )
            elif n < 1:
            	raise ValueError( "Error: n must be >= 1. n = %d" % n )
            else:
                # We adjust the dimensions of the returned image in the header:
                header['dimensions'][0] = n

                # This offset will be applied to f.seek():
                offset = idx * np.product( header['dimensions'][1:] ) * np.dtype( header['dtype'] ).itemsize

        else:
            offset = 0

        f.seek(DEFAULT_HEADER_LEN + header['extendedBytes'] + offset)
        if bool(useMemmap):
            image = np.memmap( f, dtype=header['dtype'], 
                              mode='c', 
                              shape=(header['dimensions'][0],header['dimensions'][1],header['dimensions'][2]) )
        else:
            image = np.fromfile( f, dtype=header['dtype'], 
                                count=np.product( header['dimensions'] ) )
                                

        if header['MRCtype'] == 101:
            # Seems the 4-bit is interlaced ...
            interlaced_image = image
            
            image = np.empty( np.product(header['dimensions']), dtype=header['dtype'] )
            #if bool(reverseStripes):
            #    # Bit-shift and Bit-and to seperate decimated pixels
            #    image[1::2] = np.left_shift(interlaced_image,4) / 15
            #    image[0::2] = np.right_shift(interlaced_image,4)
            #else: # Default
            image[0::2] = np.left_shift(interlaced_image,4) / 15
            image[1::2] = np.right_shift(interlaced_image,4)

        image = np.squeeze( np.reshape( image, header['dimensions'] ) )
        return image, header

       
def __MRCZImport( f, header, endian='le', fileConvention = "ccpem", returnHeader = False, n_threads=None ):
    """
    Equivalent to MRCImport, but for compressed data using the blosc library.
    
    The following compressors are supported: 
        'zlib'
        'zstd'
        'lz4' 
    
    Memory mapping is not possible in this case at present.  
    """
    if not bloscPresent:
        logger.error( "blosc not present, cannot compress files." )
        return
        
    if n_threads == None:
        blosc.nthreads = blosc.detect_number_of_cores()
    else:
        blosc.nthreads = n_threads
        
    image = np.empty( header['dimensions'], dtype=header['dtype'] )
    
    blosc_chunk_pos = DEFAULT_HEADER_LEN + header['extendedBytes']
    for J in np.arange(image.shape[0]):
        f.seek( blosc_chunk_pos )
        ( (nbytes, blockSize, ctbytes ), (ver_info) ) = readBloscHeader(f)
        f.seek(blosc_chunk_pos)
        # blosc includes the 16 header bytes in ctbytes
        image[J,:,:] = np.reshape( 
            np.frombuffer( blosc.decompress( f.read( ctbytes ) ), dtype=image.dtype ),
            image.shape[1:] )
            
        blosc_chunk_pos += (ctbytes)
        pass
    
    
    if header['MRCtype'] == 101:
        # Seems the 4-bit is interlaced 
        interlaced_image = image
            
        image = np.empty( np.product(header['dimensions']), dtype=header['dtype'] )
        # Bit-shift and Bit-and to seperate decimated pixels
        image[0::2] = np.left_shift(interlaced_image,4) / 15
        image[1::2] = np.right_shift(interlaced_image,4)

    # We don't need to reshape packed data.
    image = np.squeeze( image )
    
    return image, header
    

    
def readBloscHeader( filehandle ):
    """
    Reads in the 16 byte header file from a blosc chunk. Blosc header format 
    for each chunk is as follows:
    
    |-0-|-1-|-2-|-3-|-4-|-5-|-6-|-7-|-8-|-9-|-A-|-B-|-C-|-D-|-E-|-F-|
  ^   ^   ^   ^ |     nbytes    |   blocksize   |    ctbytes    |
  |   |   |   |
  |   |   |   +--typesize
  |   |   +------flags
  |   +----------versionlz
  +--------------version
    """
    [version, versionlz, flags, typesize] = np.fromfile( filehandle, dtype='uint8', count=4 )
    [nbytes, blocksize, ctbytes] = np.fromfile( filehandle, dtype='uint32', count=3 )
    return ( [nbytes, blocksize, ctbytes], [version, versionlz, flags, typesize] )
    
    
def readMRCHeader( MRCfilename, endian='le', fileConvention = "ccpem", pixelunits=u'\\AA' ):
    """
    Reads in the first 1024 bytes from an MRC file and parses it into a Python dictionary, yielding 
    header information.
    """
    if endian == 'le':
        endchar = '<'
    else:
        endchar = '>'
        
    header = {}
    with open( MRCfilename, 'rb' ) as f:
        # diagStr = ""
        # Get dimensions, in format [nz, ny, nx] (stored as [nx,ny,nz] in the file)
        header['dimensions'] = np.flipud( np.fromfile( f, dtype=endchar+'i4', count=3 ) )
    
        header['MRCtype'] = int( np.fromfile( f, dtype=endchar+'i4', count=1 )[0] )
        # Hack to fix lack of standard endian indication in the file header
        if header['MRCtype'] > 16000000: 
            # Endianess found to be backward
            header['MRCtype'] = int( np.asarray( header['MRCtype'] ).byteswap()[0] )
            header['dimensions'] = header['dimensions'].byteswap()
            if endchar == '<':
                endchar = '>' 
            else:
                endchar = '<'
        
        # Extract compressor from dtype > MRC_COMP_RATIO
        header['compressor'] = COMPRESSOR_ENUM[ np.floor_divide(header['MRCtype'], MRC_COMP_RATIO) ]
        header['MRCtype'] = np.mod( header['MRCtype'], MRC_COMP_RATIO )
        logger.info( "compressor: %s, MRCtype: %s" % (str(header['compressor']),str(header['MRCtype'])) )
        
        fileConvention = fileConvention.lower()
        # if fileConvention == "ccpem":
        #     diagStr += ("ioMRC.readMRCHeader: MRCtype: %s, compressor: %s, dimensions %s" % 
        #         (CCPEM_ENUM[header['MRCtype']],header['compressor'], header['dimensions'] ) )
        # elif fileConvention == "eman2":
        #     diagStr += ( "ioMRC.readMRCHeader: MRCtype: %s, compressor: %s, dimensions %s" % 
        #         (EMAN2_ENUM[header['MRCtype']],header['compressor'], header['dimensions'] ) )

        
        if fileConvention == "eman2":
            try:
                header['dtype'] = EMAN2_ENUM[ header['MRCtype'] ]
            except:
                raise ValueError( "Error: unrecognized EMAN2-MRC data type = " + str(header['MRCtype']) )
                
        elif fileConvention == "ccpem": # Default is CCPEM
            try:
                header['dtype'] = CCPEM_ENUM[ header['MRCtype'] ]
            except:
                raise ValueError( "Error: unrecognized CCPEM-MRC data type = " + str(header['MRCtype']) )
        else:
            raise ValueError( "Error: unrecognized MRC file convention: {}".format(fileConvention) )
                
        # Apply endian-ness to NumPy dtype
        header['dtype'] = endchar + header['dtype']
        # Read in pixelsize
        f.seek(40)
        cellsize = np.fromfile( f, dtype=endchar + 'f4', count=3 )
        header['pixelsize'] = np.flipud( cellsize ) / header['dimensions']
        # MRC is Angstroms by convention

        header['pixelunits'] = pixelunits
            
        # '\AA' will eventually be deprecated, please cease using it.
        if header['pixelunits'] == u"\\AA" or header['pixelunits']==u"\AA":
            pass
        elif header['pixelunits'] == u"\mum":
            header['pixelsize'] *= 1E-5
        elif header['pixelunits'] == u"pm":
            header['pixelsize'] *= 100.0
        else: # Default to nm
            header['pixelsize'] *= 0.1
            
        # Read in [X,Y,Z] array ordering
        # Currently I don't use this
        # f.seek(64)
        # axesTranpose = np.fromfile( f, dtype=endchar + 'i4', count=3 ) - 1
        
        # Read in statistics
        f.seek(76)
        (header['minImage'], header['maxImage'], header['meanImage']) = np.fromfile( f, dtype=endchar + 'f4', count=3 )

        f.seek(92)
        header['extendedBytes'] = int( np.fromfile( f, dtype=endchar + 'i4', count=1) )
        if header['extendedBytes'] > 0:
            # diagStr += ", extended header %d" % header['extendedBytes']

            f.seek(104)
            header['metaId'] = f.read(4)
            if header['metaId'] == b'json':
                f.seek(DEFAULT_HEADER_LEN)
                header.update( json.loads( f.read(header['extendedBytes'] ).decode('utf-8') ) )

        # Read in kV, C3, and gain
        f.seek(132)
        header['voltage']  = np.fromfile( f, dtype=endchar + 'f4', count=1 )
        header['C3']  = np.fromfile( f, dtype=endchar + 'f4', count=1 )
        header['gain']  = np.fromfile( f, dtype=endchar + 'f4', count=1 )
        
        #diagStr += ", voltage: %.1f, C3: %.2f, gain: %.2f" % (header['voltage'], header['C3'], header['gain']) 

        # Read in size of packed data
        f.seek(144)
        # Have to convert to Python int to avoid index warning.
        header['packedBytes'] = struct.unpack( 'q', f.read(8) )
        # header['packedBytes'] = int( np.fromfile( f, dtype=endchar + 'i8', count=1) )
        # if header['packedBytes'] > 0:
        #     diagStr += ", packedBytes: %d" % header['packedBytes']
        
        

        # How many bytes in an MRC
        return header
        
def writeMRC( input_image, MRCfilename, meta=None, endian='le', dtype=None, 
               pixelsize=[0.1,0.1,0.1], pixelunits=u"\\AA", shape=None, 
               voltage=0.0, C3=0.0, gain=1.0,
               compressor=None, clevel = 1, n_threads=None, quickStats=True, idx = None ):
    """
   writeMRC( input_image, MRCfilename, meta=None, idx=None, 
               endian='le', dtype=None, 
               pixelsize=[0.1,0.1,0.1], pixelunits=u"\\AA", shape=None, 
               voltage=0.0, C3=0.0, gain=1.0,
               compressor=None, clevel=1, n_threads=None, quickStats=True, 
               )
    Created on Thu Apr 02 15:56:34 2015
    @author: Robert A. McLeod
    
    Given a numpy 2-D or 3-D array `input_image` write it has an MRC file `MRCfilename`.

        meta is a Python dict{} which will be serialized by JSON and written 
        into the extended header.
    
        dtype will cast the data before writing it.
        
        pixelsize is [z,y,x] pixel size (singleton values are ok for square/cubic pixels)
        
        pixelunits is "AA" for Angstroms, "pm" for picometers, "\mum" for micrometers, 
        or "nm" for nanometers.  MRC standard is always Angstroms, so pixelsize 
        is converted internally from nm to Angstroms if necessary
        
        shape is only used if you want to later append to the file, such as 
        merging together Relion particles for Frealign.  Not recommended and 
        only present for legicacy reasons.
        
        voltage is accelerating potential in keV, defaults to 300.0
        
        C3 is spherical aberration in mm, defaults to 2.7 mm
        
        gain is detector gain (counts/primary electron), defaults to 1.0 (for counting camera)
        
        compressor is a choice of 'lz4', 'zlib', or 'zstd', plus 'blosclz', 'lz4hc'  
        'zstd' generally gives the best compression performance, and is still almost 
           as fast as 'lz4' with clevel = 1
        'zlib' is easiest to decompress with other utilities.
        
        clevel is the compression level, 1 is fastest, 11 is very-slow.  The compression
        ratio will rise slowly with clevel.
        
        n_threads is number of threads to use for blosc compression
        
        quickStats = True estimates the image mean, min, max from the first frame only,
        which saves a lot of computational time for stacks.

        idx can be used to write an image or set of images starting at a 
        specific position in the MRC file (which may already exist). Index of 
        first image is 0. A negative index can be used to count backwards. If 
        omitted, will write whole stack to file. If writing to an existing 
        file, compression or extended MRC2014 headers are currently not 
        supported with this option.
    
    Note that MRC definitions are not consistent.  Generally we support the CCPEM schema.
    """

    if len( input_image.shape ) == 2:
        # If it's a 2D image we force it to 3D - this makes life easier later:
        input_image = input_image.reshape( ( 1, input_image.shape[0], input_image.shape[1] ) )

    # For dask, we don't want to import dask, but we can still work-around how to 
    # check its type without isinstance()
    image_type = type(input_image)
    if image_type.__module__ == 'dask.array.core' and image_type.__name__ == 'Array':
        # Ideally it would be faster to iterate over the chunks and pass each one 
        # to blosc but that likely requires c-blosc2
        input_image = input_image.__array__()

    # We will need this regardless if writing to an existing file or not:
    if endian == 'le':
        endchar = '<'
    else:
        endchar = '>'

    # We now check if we have to create a new header (i.e. new file) or not. If 
    # the file exists, but idx is 'None', it will be replaced by a new file 
    # with new header anyway:
    if os.path.isfile( MRCfilename ):
        if idx == None:
            idxnewfile = True
        else:
            idxnewfile = False
    else:
        idxnewfile = True

    
    if idxnewfile:
        if dtype == 'uint4' and compressor != None:
            raise TypeError( "uint4 packing is not compatible with compression, use int8 datatype." )
            
        header = {'meta': meta }
        if dtype == None:
            # TODO: endian support
            header['dtype'] = endchar + input_image.dtype.descr[0][1].strip( "<>|" )
        else:
            header['dtype'] = dtype
            
        # Now we need to filter dtype to make sure it's actually acceptable to MRC
        if not header['dtype'].strip( "<>|" ) in REVERSE_CCPEM_ENUM:
            raise TypeError( "ioMRC.MRCExport: Unsupported dtype cast for MRC %s" % header['dtype'] )
            
        header['dimensions'] = input_image.shape
        
        header['pixelsize'] = pixelsize
        header['pixelunits'] = pixelunits
        header['compressor'] = compressor
        header['clevel'] = clevel
        header['shape'] = shape
        
        # This overhead calculation is annoying but many 3rd party tools that use 
        # MRC require these statistical parameters.
        if bool(quickStats) and input_image.ndim == 3:
            header['maxImage'] = np.max( np.real( input_image[0,:,:] ) )
            header['minImage'] = np.min( np.real( input_image[0,:,:] ) )
            header['maxImage'] = np.mean( np.real( input_image[0,:,:] ) )
        else:
            header['maxImage'] = np.max( np.real( input_image ) )
            header['minImage'] = np.min( np.real( input_image ) )
            header['maxImage'] = np.mean( np.real( input_image ) )
        
        header['voltage'] = voltage
        if not bool( header['voltage'] ):
            header['voltage'] = 0.0
        header['C3'] = C3
        if not bool( header['C3'] ):
            header['C3'] = 0.0
        header['gain'] = gain
        if not bool( header['gain'] ):
            header['gain'] = 1.0
        
        header['compressor'] = compressor
        header['clevel'] = clevel
        if n_threads == None and bloscPresent:
            n_threads = blosc.detect_number_of_cores()
        header['n_threads'] = n_threads
        
        # TODO: can we detect the number of cores without adding a heavy dependancy?
        
        if dtype == 'uint4':
            # Decimate to packed 4-bit
            input_image = input_image.astype('uint8')
            input_image = input_image[:,:,::2] + np.left_shift(input_image[:,:,1::2],4)

    else:
    # We are going to append to an already existing file:

        # So we try to figure out its header with 'CCPEM' or 'eman2' file conventions:
        try:
            header = readMRCHeader( MRCfilename, endian, fileConvention = 'CCPEM', pixelunits=pixelunits )

        except ValueError:
            try:
                header = readMRCHeader( MRCfilename, endian, fileConvention = 'eman2', pixelunits=pixelunits )
            except ValueError:
            # If neither 'CCPEM' nor 'eman2' formats satisfy:
                raise ValueError( "Error: unrecognized MRC type for file: %s " % MRCfilename )

        # No support for extended headers in arbitrary appending mode:
        # RAM: should work now
        # if header['extendedBytes'] > 0:
        #     raise ValueError( "Error: MRC2014 files with extended headers not supported for writing: %s = %d" % ('extendedBytes', header['extendedBytes'] ) )

        # If the file already exists, its X,Y dimensions must be consistent with the current image to be written:
        if np.any( header['dimensions'][1:] != input_image.shape[1:] ):
            raise ValueError( "Error: x,y dimensions of image do not match that of MRC file: %s " % MRCfilename )
            # TO DO: check also consistency of dtype?

    # Now that we have a proper header, we go into the details of writing to a specific position:
    if idx != None:
        if header['compressor'] != None:
            raise RuntimeError( "Writing at arbitrary positions not supported for compressed files. Compressor = %s" % header['compressor'] )

        idx = int(idx)
        # Force 2D to 3D dimensions:
        if len( header['dimensions'] ) == 2:
            header['dimensions'] = np.array( [1, header['dimensions'][0], header['dimensions'][1]] )

        # Convert negative index to equivalent positive index:
        if idx < 0:
            idx = header['dimensions'][0] + idx

        # Just check if the desired image is within the stack range:
        # In principle we could write to a position beyond the limits of the file (missing slots would be filled with zeros), but let's avoid that the user writes a big file with zeros by mistake. So only positions within or immediately consecutive to the stack are allowed:
        if idx < 0 or idx > header['dimensions'][0]:
            raise ValueError( "Error: image or slice index out of range. idx = %d, z_dimension = %d" % (idx, header['dimensions'][0]) )

        # The new Z dimension may be larger than that of the existing file, or even of the new file, if an index larger than the current stack is specified:
        newZ = idx + input_image.shape[0]
        if newZ > header['dimensions'][0]:
            header['dimensions'] = np.array( [idx + input_image.shape[0], header['dimensions'][1], header['dimensions'][2]] )

        # This offset will be applied to f.seek():
        offset = idx * np.product( header['dimensions'][1:] ) * np.dtype( header['dtype'] ).itemsize

    else:
        offset = 0
        
    __MRCExport( input_image, header, MRCfilename, endchar, offset, idxnewfile )
 
        
def __MRCExport( input_image, header, MRCfilename, endchar = '<', offset = 0, idxnewfile = True ):
    """
    MRCExport private interface with a dictionary rather than a mess of function 
    arguments.
    """

    if idxnewfile:
        # If forcing a new file we truncate it even if it already exists:
        fmode = 'wb'

    else:
        # Otherwise we'll just update its header and append images as required:
        fmode = 'rb+'

    with open( MRCfilename, fmode, buffering=BUFFERSIZE ) as f:
        extendedBytes = writeMRCHeader( f, header, endchar )
        f.seek(DEFAULT_HEADER_LEN + extendedBytes + offset)
        
        if ('compressor' in header) \
                and (header['compressor'] in REVERSE_COMPRESSOR_ENUM) \
                and (REVERSE_COMPRESSOR_ENUM[header['compressor']]) > 0:
            # compressed MRCZ
            logger.info( "Compressing %s with compressor %s%d" %
                    (MRCfilename, header['compressor'], header['clevel'] ) )
            
            if header['dtype'] != 'uint4' and input_image.dtype != header['dtype']:
                # This correctly works for text to dtype comparison
                input_image = input_image.astype(header['dtype']) 
                
            if input_image.ndim == 3:
                chunkSize = input_image[0,:,:].size
            else:
                chunkSize = input_image.size
                input_image = np.reshape( input_image, [1,input_image.shape[0],input_image.shape[1] ])
                
            blosc.set_nthreads( header['n_threads'] )
            blosc.set_blocksize( BLOSC_BLOCK )
            
            header['packedBytes'] = 0
            typeSize = input_image.dtype.itemsize
            
            for J in np.arange( input_image.shape[0] ):
                # print( "Slice %d: Compressing address at: %d of %d:" % (J, int(J*typeSize*blockSize), input_image.nbytes) )
                
                # Looks like I have problem for typesize > 1?
                if int(J*typeSize*chunkSize) >= input_image.nbytes:
                    raise MemoryError( "MRCExport: Tried to reference past end of ndarray %d > %d" % (int(J*typeSize*chunkSize), input_image.nbytes ) )
                    
                compressedData = blosc.compress( input_image[J,:,:].tobytes(),
                            typeSize, 
                            clevel=header['clevel'], 
                            shuffle=blosc.BITSHUFFLE,
                            cname=header['compressor'] )
                f.write( compressedData )
                    
                header['packedBytes'] += len(compressedData)
                
            # Rewind and write out the total compressed size
            f.seek(144)
            np.int64( header['packedBytes'] ).astype( endchar + "i8" ).tofile(f)

            
        else: # vanilla MRC
            if header['dtype'] != 'uint4' and input_image.dtype != header['dtype']:
                input_image = input_image.astype( header['dtype'] )
            input_image.tofile(f)
            
            
    return 
    
def writeMRCHeader( f, header, endchar = '<' ):
    """
    Usage:
        writeMRCHeader( f, header )
        
    Writes a  header to the file-like object `f`, requires a dict 
    called `header` to parse the appropriate fields. Use defaultHeader to see 
    all fields.
    
    http://bio3d.colorado.edu/CCPEM/doc/mrc_format.txt 
    """
        
    f.seek(0)
    # Write dimensions
    if len(header['dimensions']) == 2: # force to 3-D
        dimensions = np.array( [1, header['dimensions'][0], header['dimensions'][1]] )
    else: 
        dimensions = np.array( header['dimensions'] )
        
    # Flip to Fortran order
    dimensions = np.flipud( dimensions )
    dimensions.astype(endchar+'i4').tofile( f )
    
    # 64-bit floats are automatically down-cast
    dtype = header['dtype'].lower().strip( '<>|' )
    try:
        MRCmode = np.int32( REVERSE_CCPEM_ENUM[dtype] ).astype( endchar + "i4" )
    except:
        raise ValueError( "Warning: Unknown dtype for MRC encountered = " + str(dtype) )
        
    # Add 1000 * COMPRESSOR_ENUM to the dtype for compressed data
    if ('compressor' in header 
                and header['compressor'] in REVERSE_COMPRESSOR_ENUM 
                and REVERSE_COMPRESSOR_ENUM[header['compressor']] > 0):
        header['compressor'] = header['compressor'].lower()
        MRCmode += MRC_COMP_RATIO * REVERSE_COMPRESSOR_ENUM[ header['compressor'] ]
        
        # How many bytes in an MRCZ file, so that the file can be appended-to.
        try:
            f.seek(144)
            np.int32( header['packedBytes'] ).astype( endchar + "i8" ).tofile(f)
        except:
            # This is written afterward so we don't try to keep the entire compressed file in RAM
            pass
            
        
    f.seek(12)
    MRCmode.tofile(f)
    
    # Print NXSTART, NYSTART, NZSTART
    np.array( [0,0,0], dtype=endchar+"i4" ).tofile(f)
    # Print MX, MY, MZ, the number of pixels
    np.array( dimensions, dtype=endchar+"i4" ).tofile(f)
    # Print cellsize = pixelsize * dimensions
    # '\AA' will eventually be deprecated, please cease using it.
    if header['pixelunits'] == "\\AA" or header['pixelunits'] == '\AA':
        AApixelsize = np.array(header['pixelsize'])
    elif header['pixelunits'] == "\mum":
        AApixelsize = np.array(header['pixelsize'])*10000.0
    elif header['pixelunits'] == "pm":
        AApixelsize = np.array(header['pixelsize'])/100.0
    else: # Default is nm
        AApixelsize = np.array(header['pixelsize'])*10.0   
    
    # The above AApixelsize insures we cannot have an array of len=1 here
    if isinstance( AApixelsize, np.ndarray ) and AApixelsize.size == 1:
        cellsize = np.array( [AApixelsize,AApixelsize,AApixelsize]  ) * dimensions
    elif not isinstance( AApixelsize, (list,tuple,np.ndarray) ):
        cellsize = np.array( [AApixelsize,AApixelsize,AApixelsize]  ) * dimensions
    elif len(AApixelsize) == 2:
        # Default to z-axis pixelsize of 10.0 Angstroms
        cellsize = np.flipud(np.array( [AApixelsize[0], AApixelsize[1], 10.0] )) * dimensions
    else:
        cellsize = np.flipud(np.array( AApixelsize )) *  dimensions
        
    f.seek(40)
    np.array( cellsize, dtype=endchar+"f4" ).tofile(f)
    # Print default cell angles
    np.array( [90.0,90.0,90.0], dtype=endchar+"f4" ).tofile(f)
    
    # Print axis associations (we use C ordering internally in all Python code)
    np.array( [1,2,3], dtype=endchar+"i4").tofile(f)
    
    # Print statistics (if available)
    f.seek(76)
    if 'minImage' in header:
        np.float32( header['minImage'] ).astype(endchar+"f4").tofile(f)
    else:
        np.float32( 0.0 ).astype(endchar+"f4").tofile(f)
    if 'maxImage' in header:
        np.float32( header['maxImage'] ).astype(endchar+"f4").tofile(f)
    else:
        np.float32( 1.0 ).astype(endchar+"f4").tofile(f)
    if 'meanImage' in header:
        np.float32( header['meanImage'] ).astype(endchar+"f4").tofile(f)
    else:
        np.float32( 0.0 ).astype(endchar+"f4").tofile(f)
        
    # We'll put the compressor info and number of compressed bytes in 132-204
    # and new metadata
    # RESERVED: 132: 136: 140 : 144 for voltage, C3, and gain
    f.seek(132)
    if 'voltage' in header:
        np.float32( header['voltage'] ).astype(endchar+"f4").tofile(f)
    if 'C3' in header:
         np.float32( header['C3'] ).astype(endchar+"f4").tofile(f)
    if 'gain' in header:
        np.float32( header['gain'] ).astype(endchar+"f4").tofile(f)
        

    # Magic MAP_ indicator that tells us this is in-fact an MRC file
    f.seek( 208 )
    f.write( b"MAP " )
    # Write a machine stamp, '17,17' for big-endian or '68,65'
    # Note that the MRC format doesn't indicate the endianness of the endian 
    # identifier............................................................
    f.seek( 212 )
    if endchar == '<':
        f.write( struct.pack( 'BB', 68, 65 ) )
        # np.array( [68,65], dtype="uint8" ).tofile(f)
    else:
        f.write( struct.pack( 'BB', 17, 17 ) )
        #np.array( [17,17], dtype="uint8" ).tofile(f)
    
    # Extended header, if meta is not None
    if isinstance(header['meta'], dict):
        # Encode metadata as bytes with UTF-8
        jsonMeta = json.dumps( header['meta'] ).encode('utf-8')
        jsonLen = len(jsonMeta)
        # Length of extended header
        f.seek(92)
        f.write( struct.pack( endchar+'i', jsonLen ) )

        # 4-byte char ID string of extended metadata type
        f.seek(104)
        f.write( b'json' )

        # Go to the extended header
        f.seek(DEFAULT_HEADER_LEN)
        f.write( jsonMeta )
        return jsonLen
    
    # No extended header
    return 0

def asyncReadMRC( *args, **kwargs ):
    """
    asyncReadMRC( *args, **kwargs )
    @author: Robert A. McLeod

    Returns a `Future` object, call the method `result()` to get the image 
    and metadata.

    E.g. 
    worker = asyncReadMRC( 'someones_file.mrc' )
    # Do some work
    mrcImage, mrcMeta = worker.result()

    Valid arguments are as for readMRC().
    """
    return _asyncExecutor.submit( readMRC, *args, **kwargs )

def asyncWriteMRC( *args, **kwargs ):
    """
    asyncWriteMRC( *args, **kwargs )
    @author: Robert A. McLeod

    Calls writeMRC in a seperate thread and executes it in the background. Returns the thread 
    object, so if necessary you can call `Future.result()` to wait for the write to finish, 
    or check with `Future.done()`.abs

        worker = asyncWriteMRC( npImageData, 'my_mrcfile.mrc' )
        # Do some work
        if not worker.done():
            time.sleep(0.001)
        # File is written to disk

    Most of the time you can ignore the return and let the system finish writing 
    when it finishes.  
 
    Valid arguments are as for writeMRC().
    """
    #bgThread = threading.Thread( target=writeMRC, args=args, kwargs=kwargs )
    #bgThread.start()
    #return bgThread
    return _asyncExecutor.submit( writeMRC, *args, **kwargs )