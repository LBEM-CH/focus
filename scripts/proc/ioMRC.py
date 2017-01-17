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

import os, os.path
import numpy as np
try: 
    import blosc
    # Block size is set at compile time to fit into L1 cache
    bloscPresent = True
except:
    bloscPresent = False
    print( "blosc compressor not found, MRCZ format disabled." )

# Buffer for file I?O
# Quite arbitrary, in bytes (hand-optimized)
BUFFERSIZE = 2**20

# ENUM dicts for our various Python to MRC constant conversions
COMPRESSOR_ENUM = { 0:None, 1:'blosclz', 2:'lz4', 3:'lz4hc', 4:'snappy', 5:'zlib', 6:'zstd' }
REVERSE_COMPRESSOR_ENUM = { None:0, 'blosclz':1, 'lz4':2, 'lz4hc':2, 'snappy':4, 'zlib':5, 'zstd':6 }

MRC_COMP_RATIO = 1000  
IMOD_ENUM = { 0: 'i1', 1:'i2', 2:'f4', 4:'c8', 6:'u2', 101:'u1' }
EMAN2_ENUM = { 1: 'i1', 2:'u1', 3:'i2', 4:'u2', 5:'i4', 6:'u4', 7:'f4' }
REVERSE_IMOD_ENUM = { 'int8':0, 'i1':0, 
                      'uint4':101, 
                      'int16':1, 'i2':1,
                      'uint16':6, 'u2':6,
                      'float64':2, 'f8':2, 'float32':2, 'f4':2,
                      'complex128':4, 'c16':4, 'complex64':4, 'c8':4 }

def defaultHeader( ):
    """
    Returns a default MRC header dictionary with all fields with default values.
    """
    header = {}
    header['fileConvention'] = "imod"
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
    header['pixelunits'] = u"nm" # Can be "\AA" for Angstroms
    header['voltage'] = 300.0 # kV
    header['C3'] = 2.7 # mm
    header['gain'] = 1.0 # counts/electron
    
    if bloscPresent:
        header['n_threads'] = blosc.detect_number_of_cores()
    
    return header
    
    

#def MRCImport( MRCfilename, useMemmap = False, endian='le', 
#              pixelunits=u'nm', fileConvention = "imod", 
#              n_threads = None ):
    
    
def readMRC( MRCfilename, useMemmap = False, endian='le', 
              pixelunits=u'\AA', fileConvention = "imod", 
              n_threads = None ):
    """
    readMRC
    
    Created on Thu Apr 09 11:05:07 2015
    @author: Robert A. McLeod
    
    Imports an MRC/Z file as a NumPy array and a meta-data dict.  
    
    Usage:
        
    [data, header] = readMRC( "my_filename.mrcz", seMemmap = False, endian='le', 
              pixelunits=u'\AA', fileConvention = "imod", n_threads = None )
    
    * data is a NumPy array.
        
    * header is a dict with various fields relating to the MRC header information.
    
    * pixelunits can be '\AA' (Angstoms), 'nm', '\mum', or 'pm'.  Internally pixel 
      sizes are always encoded in Angstroms in the MRC file.
        
    * fileConvention can be 'imod' (equivalent to CCP4) or 'eman2', which is
      only partially supported at present.
    
     * endian can be big-endian as 'be' or little-endian as 'le'
        
     * n_threads is the number of threads to use for decompression, defaults to 
       all virtual cores.
        
    """
    with open( MRCfilename, 'rb', buffering=BUFFERSIZE ) as f:
        # Read in header as a dict
        
        header = readMRCHeader( MRCfilename, endian=endian, fileConvention = "imod", pixelunits=pixelunits )
        # Support for compressed data in MRCZ

        
        if ( (header['compressor'] in REVERSE_COMPRESSOR_ENUM) 
            and (REVERSE_COMPRESSOR_ENUM[header['compressor']] > 0) ):
            return __MRCZImport( f, header, endian=endian, fileConvention = fileConvention, 
                                n_threads=n_threads )
        # Else save as MRC file
                                
        f.seek(1024 + header['extendedBytes'])
        if bool(useMemmap):
            image = np.memmap( f, dtype=header['dtype'], 
                              mode='c', 
                              shape=(header['dimensions'][0],header['dimensions'][1],header['dimensions'][2]) )
        else:
            image = np.fromfile( f, dtype=header['dtype'], 
                                count=np.product(header['dimensions']) )
                                
        # print( "DEBUG 1: ioMRC.MRCImport # nans: %d" % np.sum(np.isnan(image)) )    
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

        # print( "DEBUG 2: ioMRC.MRCImport # nans: %d" % np.sum(np.isnan(image)) )
        image = np.squeeze( np.reshape( image, header['dimensions'] ) )
        # print( "DEBUG 3: ioMRC.MRCImport # nans: %d" % np.sum(np.isnan(image)) )
        return image, header

       
def __MRCZImport( f, header, endian='le', fileConvention = "imod", returnHeader = False, n_threads=None ):
    """
    Equivalent to MRCImport, but for compressed data using the blosc library.
    
    The following compressors are supported: 
        'zlib'
        'zstd'
        'lz4' 
    
    Memory mapping is not possible in this case at present.  
    

    """
    if not bloscPresent:
        print( "ioMRC: blosc not present, cannot compress files." )
        return
        
    if n_threads == None:
        blosc.nthreads = blosc.detect_number_of_cores()
    else:
        blosc.nthreads = n_threads
        
    image = np.empty( header['dimensions'], dtype=header['dtype'] )
    
    # We can read MRC2014 files that don't start at 1024 bytes, but not write them 
    # (as they are non-standard and we don't like breaking stuff)
    blosc_chunk_pos = 1024 + header['extendedBytes']
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
    
    
def readMRCHeader( MRCfilename, endian='le', fileConvention = "imod", pixelunits=u'\AA' ):
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
        diagStr = ""
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
        print( "compressor: %s, MRCtype: %s" % (str(header['compressor']),str(header['MRCtype'])) )
        
        fileConvention = fileConvention.lower()
        if fileConvention == "imod":
            diagStr += ("ioMRC.readMRCHeader: MRCtype: %s, compressor: %s, dimensions %s" % 
                (IMOD_ENUM[header['MRCtype']],header['compressor'], header['dimensions'] ) )
        elif fileConvention == "eman2":
            diagStr += ( "ioMRC.readMRCHeader: MRCtype: %s, compressor: %s, dimensions %s" % 
                (EMAN2_ENUM[header['MRCtype']],header['compressor'], header['dimensions'] ) )

        
        if fileConvention == "eman2":
            try:
                header['dtype'] = EMAN2_ENUM[ header['MRCtype'] ]
            except:
                raise ValueError( "Error: unrecognized EMAN2-MRC data type = " + str(header['MRCtype']) )
                
        elif fileConvention == "imod": # Default is imod
            try:
                header['dtype'] = IMOD_ENUM[ header['MRCtype'] ]
            except:
                raise ValueError( "Error: unrecognized IMOD-MRC data type = " + str(header['MRCtype']) )
                
        # Apply endian-ness to NumPy dtype
        header['dtype'] = endchar + header['dtype']
        # Read in pixelsize
        f.seek(40)
        cellsize = np.fromfile( f, dtype=endchar + 'f4', count=3 )
        header['pixelsize'] = np.flipud( cellsize ) / header['dimensions']
        # MRC is Angstroms by convention

        header['pixelunits'] = pixelunits
            

        if header['pixelunits'] == u"\AA":
            pass
        elif header['pixelunits'] == u"\mum":
            header['pixelsize'] *= 1E-5
        elif header['pixelunits'] == u"pm":
            header['pixelsize'] *= 100.0
        else: # Default to nm
            header['pixelsize'] *= 0.1
            
        # Read in [X,Y,Z] array ordering
        f.seek(64)
        # Currently I don't use this
        # axesTranpose = np.fromfile( f, dtype=endchar + 'i4', count=3 ) - 1
        
        # Read in statistics
        f.seek(76)
        (header['minImage'], header['maxImage'], header['meanImage']) = np.fromfile( f, dtype=endchar + 'f4', count=3 )

        f.seek(92)
        header['extendedBytes'] = int( np.fromfile( f, dtype=endchar + 'i4', count=1) )
        if header['extendedBytes'] > 0:
            diagStr += ", extended header %d" % header['extendedBytes']

        # Read in kV, C3, and gain
        f.seek(132)
        header['voltage']  = np.fromfile( f, dtype=endchar + 'f4', count=1 )
        header['C3']  = np.fromfile( f, dtype=endchar + 'f4', count=1 )
        header['gain']  = np.fromfile( f, dtype=endchar + 'f4', count=1 )
        
        diagStr += ", voltage: %.1f, C3: %.2f, gain: %.2f" % (header['voltage'], header['C3'], header['gain']) 

        # Read in size of packed data
        f.seek(144)
        # Have to convert to Python int to avoid index warning.
        header['packedBytes'] = int( np.fromfile( f, dtype=endchar + 'i8', count=1) )
        if header['packedBytes'] > 0:
            diagStr += ", packedBytes: %d" % header['packedBytes']
        
        # How many bytes in an MRC
        return header
        
def writeMRC( input_image, MRCfilename, endian='le', dtype=None, 
               pixelsize=[0.1,0.1,0.1], pixelunits=u"\AA", shape=None, 
               voltage = 0.0, C3 = 0.0, gain = 1.0,
               compressor=None, clevel = 1, n_threads=None, quickStats=True ):
    """
    MRCExport( input_image, MRCfilename, endian='le', shape=None, compressor=None, clevel = 1 )
    Created on Thu Apr 02 15:56:34 2015
    @author: Robert A. McLeod
    
    Given a numpy 2-D or 3-D array `input_image` write it has an MRC file `MRCfilename`.
    
        dtype will cast the data before writing it.
        
        pixelsize is [z,y,x] pixel size (singleton values are ok for square/cubic pixels)
        
        pixelunits is "AA" for Angstroms, "pm" for picometers, "\mum" for micrometers, 
        or "nm" for nanometers.  MRC standard is always Angstroms, so pixelsize 
        is converted internally from nm to Angstroms if necessary
        
        shape is only used if you want to later append to the file, such as merging together Relion particles
        for Frealign.  Not recommended and only present for legicacy reasons.
        
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
    
    Note that MRC definitions are not consistent.  Generally we support the IMOD schema.
    """

    if dtype == 'uint4' and compressor != None:
        raise TypeError( "uint4 packing is not compatible with compression, use int8 datatype." )
        
    header = {}
    if endian == 'le':
        endchar = '<'
    else:
        endchar = '>'
    if dtype == None:
        # TODO: endian support
        header['dtype'] = endchar + input_image.dtype.descr[0][1].strip( "<>|" )
    else:
        header['dtype'] = dtype
        
    # Now we need to filter dtype to make sure it's actually acceptable to MRC
    if not header['dtype'].strip( "<>|" ) in REVERSE_IMOD_ENUM:
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
        
    __MRCExport( input_image, header, MRCfilename, endchar )
 
        
def __MRCExport( input_image, header, MRCfilename, endchar = '<' ):
    """
    MRCExport private interface with a dictionary rather than a mess of function 
    arguments.
    """
    with open( MRCfilename, 'wb', buffering=BUFFERSIZE ) as f:
    
        writeMRCHeader( f, header, endchar )
        f.seek(1024)
        
        if ('compressor' in header) \
                and (header['compressor'] in REVERSE_COMPRESSOR_ENUM) \
                and (REVERSE_COMPRESSOR_ENUM[header['compressor']]) > 0:
            # compressed MRCZ
            print( "Compressing %s with compressor %s%d" %
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
            blosc.set_blocksize( 65536 )
            
            header['packedBytes'] = 0
            typeSize = input_image.dtype.itemsize
            
            print( input_image.shape )
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
                # print( "packedBytes = %d" % header['packedBytes'] )
                
            # print( "Finished writing out compressedData" )
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
        
    Writes a 1024-byte header to the file-like object `f`, requires a dict 
    called `header` to parse the appropriate fields. Use defaultHeader to see 
    all fields.
    
    http://bio3d.colorado.edu/imod/doc/mrc_format.txt 
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
        MRCmode = np.int64( REVERSE_IMOD_ENUM[dtype] ).astype( endchar + "i4" )
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
            np.int64( header['packedBytes'] ).astype( endchar + "i8" ).tofile(f)
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
    if header['pixelunits'] == "\AA":
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
        

    print( "DEBUG A" )
    # Magic MAP_ indicator that tells us this is in-fact an MRC file
    f.seek( 208 )
    np.array( b"MAP ", dtype="|S" ).tofile(f)
    # Write a machine stamp, '\x17\x17' for big-endian or '\x68\x65
    f.seek( 212 )
    if endchar == '<':
        np.array( [68,65], dtype="uint8" ).tofile(f)
    else:
        np.array( [17,17], dtype="uint8" ).tofile(f)
    

    return  