from __future__ import division, print_function, absolute_import, unicode_literals
'''
Conventional MRC2014 and on-the-fly compressed MRCZ file interface

CCPEM MRC2014 specification:
http://www.ccpem.ac.uk/mrc_format/mrc2014.php

IMOD specification:
http://bio3d.colorado.edu/imod/doc/mrc_format.txt

Testing:
http://emportal.nysbc.org/mrc2014/

Tested output on: Gatan GMS, IMOD, Chimera, Relion, MotionCorr, UnBlur
'''

import os, os.path, sys
import numpy as np
import threading
import struct
try:
    from concurrent.futures import ThreadPoolExecutor
except ImportError as e:
    if sys.version_info > (3,0):
        raise ImportError('Get the backport for `concurrent.futures` for Py2.7 as `pip install futures`')
    raise e
from mrcz.__version__ import __version__
from distutils.version import StrictVersion

import logging
logger = logging.getLogger('MRCZ')
try: 
    import blosc
    BLOSC_PRESENT = True
    # For async operations we want to release the GIL in blosc operations and 
    # file IO operations.
    blosc.set_releasegil(True)
    DEFAULT_N_THREADS = blosc.detect_number_of_cores()
except ImportError: 
    # Can be ImportError or ModuleNotFoundError depending on the Python version,
    # but ModuleNotFoundError is a child of ImportError and is still caught.
    BLOSC_PRESENT = False
    logger.info('`blosc` meta-compression library not found, file compression disabled.')
    DEFAULT_N_THREADS = 1
try: 
    import rapidjson as json
except ImportError:
    import json
    logger.info('`python-rapidjson` not found, using builtin `json` instead.')

def _defaultMetaSerialize(value):
    """
    Is called by `json.dumps()` whenever it encounters an object it does 
    not know how to serialize.
    """
    if hasattr(value, '__array_interface__'):
        # Checking for '__array_interface__' also sanitizes numpy scalars
        # like np.float32 or np.int32
        return value.tolist()
    else:
        raise TypeError('Unhandled type for JSON serialization: {}'.format(type(value)))

# Do we also want to convert long lists to np.ndarrays?

# Buffer for file I/O
# Quite arbitrary, in bytes (hand-optimized)
BUFFERSIZE = 2**20
BLOSC_BLOCK = 2**16
DEFAULT_HEADER_LEN = 1024

# ENUM dicts for our various Python to MRC constant conversions
COMPRESSOR_ENUM = {0:None, 1:'blosclz', 2:'lz4', 3:'lz4hc', 4:'snappy', 5:'zlib', 6:'zstd'}
REVERSE_COMPRESSOR_ENUM = {None:0, 'blosclz':1, 'lz4':2, 'lz4hc':2, 'snappy':4, 'zlib':5, 'zstd':6}

MRC_COMP_RATIO = 1000  
CCPEM_ENUM = {0: 'i1', 1:'i2', 2:'f4', 4:'c8', 6:'u2', 7:'i4', 8:'u4', 101:'u1'}
EMAN2_ENUM = {1: 'i1', 2:'u1', 3:'i2', 4:'u2', 5:'i4', 6:'u4', 7:'f4'}
REVERSE_CCPEM_ENUM = {'int8':0, 'i1':0, 
                      'uint4':101, 
                      'int16':1, 'i2':1,
                      'uint16':6, 'u2':6,
                      'int32':7, 'i4':7,
                      'uint32': 8, 'u4':8,
                      'float64':2, 'f8':2, 'float32':2, 'f4':2,
                      'complex128':4, 'c16':4, 'complex64':4, 'c8':4}
WARNED_ABOUT_CASTING_F64  = False
WARNED_ABOUT_CASTING_C128 = False

# Executor for writing compressed blocks to disk
_asyncWriter = ThreadPoolExecutor(max_workers = 1)
# Executor for calls to asyncReadMRC and asyncWriteMRC
_asyncExecutor = ThreadPoolExecutor(max_workers = 1)
def _setAsyncWorkers(N_workers):
    '''
    **This function is protected as there appears to be little value in using more 
    than one worker. It may be subject to removal in the future.**

    Sets the maximum number of background workers that can be used for reading 
    or writing with the functions.  Defaults to 1. Generally when writing 
    to hard drives use 1 worker. For random-access drives there may be 
    advantages to using multiple workers.
    
    Some test results, 30 files, each 10 x 2048 x 2048 x float-32, on a CPU
    with 4 physical cores:

        HD, 1 worker:   42.0 s
        HD, 2 workers:  50.0 s
        HD, 4 workers:  50.4 s
        SSD, 1 worker:  12.6 s
        SSD, 2 workers: 11.6 s
        SSD, 4 workers:  8.9 s
        SSD, 8 workers: 16.9 s

    Parameters
    ----------
    N_workers: int
        The number of threads for asynchronous reading and writing to disk
    '''
    if N_workers <= 0:
        raise ValueError('N_workers must be greater than 0')
    if _asyncExecutor._max_workers == N_workers:
        return
    _asyncExecutor._max_workers = N_workers
    _asyncExecutor._adjust_thread_count()

def setDefaultThreads(n_threads):
    """
    Set the default number of threads, if the argument is not provided in 
    calls to `readMRC` and `writeMRC`.

    Generally optimal thread count is the number of physical cores, but 
    blosc defaults to the number of virtual cores. Therefore on machines with 
    hyperthreading it can be more efficient to manually set this value
    """
    global DEFAULT_N_THREADS
    DEFAULT_N_THREADS = int(n_threads)

def defaultHeader():
    '''
    Generator function to create a metadata header dictionary with the relevant
    fields.

    Returns
    -------
    header
        a default MRC header dictionary with all fields with default values.
    '''
    header = {}
    header['fileConvention'] = 'ccpem'
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
    header['pixelunits'] = u'nm' # Can be '\\AA' for Angstroms
    header['voltage'] = 300.0 # kV
    header['C3'] = 2.7 # mm
    header['gain'] = 1.0 # counts/electron
    
    if BLOSC_PRESENT:
        header['n_threads'] = DEFAULT_N_THREADS
    
    return header

def _getMRCZVersion(label):
    """
    Checks to see if the first label holds the MRCZ version information, 
    in which case it returns a version object. Generally used to recover nicely
    in case of backward compatibility problems.

    Parameters
    ----------
    label: Union[str, bytes]

    Returns
    -------
    version: Optional[distutils.version.StrictVersion]
        areturns ``None`` if `label` cannot be parsed.
    """
    if isinstance(label, bytes):
        label = label.decode()

    label = label.rstrip(' \t\r\n\0')
    if not label.startswith('MRCZ'):
        return None

    label = label[4:]
    try:
        version = StrictVersion(label)
        return version
    except ValueError:
        return None
    
    
def readMRC(MRCfilename, idx=None, endian='le', 
            pixelunits=u'\\AA', fileConvention='ccpem', 
            useMemmap=False, n_threads=None, slices=None):
    '''
    Imports an MRC/Z file as a NumPy array and a meta-data dict.  
    
    Parameters
    ----------
    image: numpy.ndarray
        a 1-3 dimension ``numpy.ndarray`` with one of the supported data types in 
        ``mrcz.REVERSE_CCPEM_ENUM``
    meta: dict
        a ``dict`` with various fields relating to the MRC header information. 
        Can also hold arbitrary meta-data, but the use of large numerical data 
        is not recommended as it is encoded as text via JSON.
    idx: Tuple[int]
        Index tuple ``(first, last)`` where first (inclusive) and last (not 
        inclusive) indices of images to be read from the stack. Index of first image 
        is 0. Negative indices can be used to count backwards. A singleton integer 
        can be provided to read only one image. If omitted, will read whole file. 
        Compression is currently not supported with this option.
    pixelunits: str
        can be ``'AA' (Angstoms), 'nm', '\mum', or 'pm'``.  Internally pixel 
        sizes are always encoded in Angstroms in the MRC file.
    fileConvention: str 
        can be ``'ccpem'`` (equivalent to IMOD) or ``'eman2'``, which is
        only partially supported at present.
    endian: str
        can be big-endian as ``'be'`` or little-endian as ``'le'``. Defaults 
        to `'le'` as the vast majority of modern computers are little-endian.
    n_threads: int 
        is the number of threads to use for decompression, defaults to 
        use all virtual cores.
    useMemmap: bool = True
        returns a ``numpy.memmap`` instead of a ``numpy.ndarray``. Not recommended
        as it will not work with compression.
    slices: Optional[int] = None
        Reflects the number of slices per frame. For example, in time-series 
        with multi-channel STEM, would be ``4`` for a 4-quadrant detector. Data 
        is always written contiguously in MRC, but will be returned as a list of 
        ``[slices, *shape]``-shaped arrays. The default option ``None`` will 
        check for a ``'slices'`` field in the meta-data and use that, otherwise 
        it defaults to ``0`` which is one 3D array.


    Returns
    -------
    image: Union[list[numpy.ndarray], numpy.ndarray]
        If ``slices == 0`` then a monolithic array is returned, else a ``list``
        of ``[slices, *shape]``-shaped arrays.
    meta: dict
        the stored meta-data in a dictionary. Note that arrays are generally 
        returned as lists due to the JSON serialization.

    Example
    -------
        [image, meta] = readMRC(MRCfilename, idx=None, 
              pixelunits=u'\\AA', useMemmap=False, n_threads=None)
    '''

    with open(MRCfilename, 'rb', buffering=BUFFERSIZE) as f:
        # Read in header as a dict
        
        header, slices = readMRCHeader(MRCfilename, slices, endian=endian, fileConvention = fileConvention, pixelunits=pixelunits)
        # Support for compressed data in MRCZ

        
        if ( (header['compressor'] in REVERSE_COMPRESSOR_ENUM) 
            and (REVERSE_COMPRESSOR_ENUM[header['compressor']] > 0) 
            and idx == None ):
            return __MRCZImport(f, header, slices, endian=endian, fileConvention=fileConvention, 
                                n_threads=n_threads)
        # Else load as uncompressed MRC file

        if idx != None:
        # If specific images were requested:
        # TO DO: add support to read all images within a range at once

            if header['compressor'] != None:
                raise RuntimeError('Reading from arbitrary positions not supported for compressed files. Compressor = %s'%header['compressor'])
            if np.isscalar( idx ):
                indices = np.array([idx, idx], dtype='int')
            else:
                indices = np.array(idx, dtype='int')

            # Convert to old way:
            idx = indices[0]
            n = indices[1] - indices[0] + 1

            if idx < 0:
                # Convert negative index to equivalent positive index:
                idx = header['dimensions'][0] + idx

            # Just check if the desired image is within the stack range:
            if idx < 0 or idx >= header['dimensions'][0]:
                raise ValueError('Error: image or slice index out of range. idx = %d, z_dimension = %d'%(idx, header['dimensions'][0]))
            elif idx + n > header['dimensions'][0]:
            	raise ValueError('Error: image or slice index out of range. idx + n = %d, z_dimension = %d'%(idx + n, header['dimensions'][0]))
            elif n < 1:
            	raise ValueError('Error: n must be >= 1. n = %d'%n)
            else:
                # We adjust the dimensions of the returned image in the header:
                header['dimensions'][0] = n

                # This offset will be applied to f.seek():
                offset = idx * np.product(header['dimensions'][1:])*np.dtype(header['dtype']).itemsize

        else:
            offset = 0

        f.seek(DEFAULT_HEADER_LEN + header['extendedBytes'] + offset)
        if bool(useMemmap):
            image = np.memmap(f, dtype=header['dtype'], 
                              mode='c', 
                              shape=tuple(dim for dim in header['dimensions']))
        else: # Load entire file into memory
            dims = header['dimensions']
            if slices > 0: # List of NumPy 2D-arrays
                frame_size = slices * np.product(dims[1:])
                n_frames = dims[0] // slices
                dtype = header['dtype']

                # np.fromfile advances the file pointer `f` for us.
                image = [np.squeeze(np.fromfile(f, dtype=dtype, count=frame_size).reshape((slices, dims[1], dims[2]))) \
                        for imSlice in range(n_frames)]
               
            else: # monolithic NumPy ndarray
                image = np.fromfile(f, dtype=header['dtype'], count=np.product(dims))

                if header['MRCtype'] == 101:
                    # Seems the 4-bit is interlaced ...
                    interlaced_image = image
                    image = np.empty(np.product(dims), dtype=header['dtype'])
                    image[0::2] = np.left_shift(interlaced_image,4) / 15
                    image[1::2] = np.right_shift(interlaced_image,4)

                image = np.squeeze(image.reshape(dims))

        return image, header

       
def __MRCZImport(f, header, slices, endian='le', fileConvention='ccpem', 
                 returnHeader=False, n_threads=None):
    '''
    Equivalent to MRCImport, but for compressed data using the blosc library.
    
    The following compressors are recommended: [``'zlib'``, ``'zstd'``, ``'lz4'``]
        
    Memory mapping is not possible in this case at present. Possibly support can
    be added for memory mapping with `c-blosc2`.
    '''
    if not BLOSC_PRESENT:
        raise ImportError( '`blosc` is not installed, cannot decompress file.' )
        
    if n_threads == None:
        blosc.nthreads = DEFAULT_N_THREADS
    else:
        blosc.nthreads = n_threads

    dims = header['dimensions']
    dtype = header['dtype']
    if slices > 0:
        image = []
        n_frames = dims[0] // slices
    else:
        image = np.empty(dims, dtype=dtype)
        n_frames = dims[0]
    
    if slices > 1:
        target_shape = (slices, dims[1], dims[2])
    else:
        target_shape = (dims[1], dims[2])
    # target_shape = (dims[1], dims[2])
    blosc_chunk_pos = DEFAULT_HEADER_LEN + header['extendedBytes']

    # NOTE: each channel of each frame is separately compressed by blosc, 
    # so that if slices is not what was originally input, each slice can 
    # be decompressed individually.
    if slices == 1: # List of 2D frames
        for J in range(n_frames):
            f.seek(blosc_chunk_pos)
            ((nbytes, blockSize, ctbytes), (ver_info)) = readBloscHeader(f)
            f.seek(blosc_chunk_pos)
            image.append(np.frombuffer(
                        blosc.decompress(f.read(ctbytes)), dtype=dtype).reshape(target_shape))
            blosc_chunk_pos += (ctbytes)
    elif slices > 1: # List of 3D frames
        for J in range(n_frames):
            frame = np.empty(target_shape, dtype=dtype)
            for I in range(slices):
                f.seek(blosc_chunk_pos)
                ((nbytes, blockSize, ctbytes), (ver_info)) = readBloscHeader(f)
                f.seek(blosc_chunk_pos)
                frame[I,:,:] = np.frombuffer(
                        blosc.decompress(f.read(ctbytes)), dtype=dtype).reshape(target_shape[1:])
                blosc_chunk_pos += (ctbytes)
            image.append(frame)
    else: # Monolithic frame
        for J in range(n_frames):
            f.seek(blosc_chunk_pos)
            ((nbytes, blockSize, ctbytes), (ver_info)) = readBloscHeader(f)
            f.seek(blosc_chunk_pos)
            image[J,:,:] = np.frombuffer(
                                    blosc.decompress(f.read(ctbytes)), dtype=dtype).reshape(target_shape)
            blosc_chunk_pos += (ctbytes)
    
    
    if header['MRCtype'] == 101:
        # Seems the 4-bit is interlaced 
        if slices > 0:
            raise NotImplementedError('MRC type 101 (uint4) not supported with return as `list`')
        interlaced_image = image
            
        image = np.empty(np.product(header['dimensions']), dtype=dtype)
        # Bit-shift and Bit-and to seperate decimated pixels
        image[0::2] = np.left_shift(interlaced_image, 4) / 15
        image[1::2] = np.right_shift(interlaced_image, 4)

    if not slices > 0:
        image = np.squeeze(image)
    
    return image, header
    

def readBloscHeader(filehandle):
    '''
    Reads in the 16 byte header file from a blosc chunk. Blosc header format 
    for each chunk is as follows::
    
        |-0-|-1-|-2-|-3-|-4-|-5-|-6-|-7-|-8-|-9-|-A-|-B-|-C-|-D-|-E-|-F-|
        ^   ^   ^   ^ |     nbytes    |   blocksize   |    ctbytes    |
        |   |   |   |
        |   |   |   +--typesize
        |   |   +------flags
        |   +----------versionlz
        +--------------version
    '''
    [version, versionlz, flags, typesize] = np.fromfile(filehandle, dtype='uint8', count=4)
    [nbytes, blocksize, ctbytes] = np.fromfile(filehandle, dtype='uint32', count=3)
    return ([nbytes, blocksize, ctbytes], [version, versionlz, flags, typesize])
    
    
def readMRCHeader(MRCfilename, slices=None, endian='le', fileConvention = 'ccpem', pixelunits=u'\\AA'):
    '''
    Reads in the first 1024 bytes from an MRC file and parses it into a Python 
    dictionary, yielding header information. This function is not intended to be 
    called by the user under typical usage.

    Parameters
    ----------
    As per `readMRC`

    Returns
    -------
    header: dict
        All found meta-data in the header and extended header packaged into 
        a dictionary.
    '''
    if endian == 'le':
        endchar = '<'
    else:
        endchar = '>'
    dtype_i4 = np.dtype(endchar + 'i4')
    dtype_f4 = np.dtype(endchar + 'f4')

    header = {}
    with open(MRCfilename, 'rb') as f:
        # Grab version information early
        f.seek(224)
        mrcz_version = _getMRCZVersion(f.read(80))

        # diagStr = ''
        # Get dimensions, in format [nz, ny, nx] (stored as [nx,ny,nz] in the file)
        f.seek(0)
        header['dimensions'] = np.flipud(np.fromfile(f, dtype=dtype_i4, count=3))
    
        header['MRCtype'] = int(np.fromfile(f, dtype=dtype_i4, count=1)[0])
        # Hack to fix lack of standard endian indication in the file header
        if header['MRCtype'] > 16000000: 
            # Endianess found to be backward
            header['MRCtype'] = int(np.asarray(header['MRCtype']).byteswap()[0])
            header['dimensions'] = header['dimensions'].byteswap()
            if endchar == '<':
                endchar = '>' 
            else:
                endchar = '<'
            dtype_i4 = np.dtype(endchar + 'i4')
            dtype_f4 = np.dtype(endchar + 'f4')
        
        # Extract compressor from dtype > MRC_COMP_RATIO
        header['compressor'] = COMPRESSOR_ENUM[np.floor_divide(header['MRCtype'], MRC_COMP_RATIO)]
        header['MRCtype'] = np.mod(header['MRCtype'], MRC_COMP_RATIO)
        logger.debug('compressor: %s, MRCtype: %s' % (str(header['compressor']),str(header['MRCtype'])))
        
        fileConvention = fileConvention.lower()
        
        if fileConvention == 'eman2':
            try:
                header['dtype'] = EMAN2_ENUM[header['MRCtype']]
            except:
                raise ValueError('Error: unrecognized EMAN2-MRC data type = ' + str(header['MRCtype']))
                
        elif fileConvention == 'ccpem': # Default is CCPEM
            try:
                header['dtype'] = CCPEM_ENUM[header['MRCtype']]
            except:
                raise ValueError('Error: unrecognized CCPEM-MRC data type = ' + str(header['MRCtype']))
        else:
            raise ValueError('Error: unrecognized MRC file convention: {}'.format(fileConvention))
                
        # Apply endian-ness to NumPy dtype
        header['dtype'] = endchar + header['dtype']

        # slices is z-axis per frame for list-of-arrays representation

        if slices is None:
            # We had a bug in version <= 0.4.1 where we wrote the dimensions 
            # into both (Nx, Ny, Nz) AND (Mx, My, Mz), therefore the slicing 
            # is essentially unknown (and wrong). So we have this version 
            # check where we force slices to be 1 (i.e. we assume it is a 
            # stack of 2D images).
            if mrcz_version is not None and mrcz_version < StrictVersion('0.5.0'):
                logger.warning('MRCZ version < 0.5.0 for file {}, assuming slices == 1.'.format(MRCfilename))
                slices = 1
            else:
                f.seek(36)
                slices = int(np.fromfile(f, dtype=dtype_i4, count=1))

        # Read in pixelsize
        f.seek(40)
        cellsize = np.fromfile(f, dtype=dtype_f4, count=3)
        header['pixelsize'] = np.flipud( cellsize ) / header['dimensions']
        # MRC is Angstroms by convention

        header['pixelunits'] = pixelunits
            
        # '\AA' will eventually be deprecated, please cease using it.
        if header['pixelunits'] == u'\\AA' or header['pixelunits'] == u'\AA':
            pass
        elif header['pixelunits'] == u'\mum':
            header['pixelsize'] *= 1E-5
        elif header['pixelunits'] == u'pm':
            header['pixelsize'] *= 100.0
        else: # Default to nm
            header['pixelsize'] *= 0.1
            
        # Read in [X,Y,Z] array ordering
        # Currently I don't use this
        # f.seek(64)
        # axesTranpose = np.fromfile( f, dtype=endchar + 'i4', count=3 ) - 1
        
        # Read in statistics
        f.seek(76)
        header['minImage'], header['maxImage'], header['meanImage'] = np.fromfile(f, dtype=dtype_f4, count=3)

        # Size of meta-data
        f.seek(92)
        header['extendedBytes'] = int(np.fromfile(f, dtype=dtype_i4, count=1))
        if header['extendedBytes'] > 0:
            f.seek(104)
            header['metaId'] = f.read(4)
            
        # Read in kV, C3, and gain
        f.seek(132)
        microscope_state = np.fromfile(f, dtype=dtype_f4, count=3)
        header['voltage'] = float(microscope_state[0])
        header['C3']      = float(microscope_state[1])
        header['gain']    = float(microscope_state[2])
            
        # Read in size of packed data
        f.seek(144)
        # Have to convert to Python int to avoid index warning.
        header['packedBytes'] = struct.unpack('q', f.read(8))

        # Now read in JSON meta-data if present
        if 'metaId' in header and header['metaId'] == b'json':
                f.seek(DEFAULT_HEADER_LEN)
                header.update(json.loads(f.read(header['extendedBytes'] ).decode('utf-8')))

        return header, slices
        
def writeMRC(input_image, MRCfilename, meta=None, endian='le', dtype=None, 
             pixelsize=[0.1,0.1,0.1], pixelunits=u'\\AA', shape=None, 
             voltage=0.0, C3=0.0, gain=1.0, compressor=None, clevel=1, 
             n_threads=None, quickStats=True, idx=None):
    '''
    Write a conventional MRC file, or a compressed MRCZ file to disk.  If 
    compressor is ``None``, then backwards compatibility with other MRC libraries
    should be preserved.  Other libraries will not, however, recognize 
    the JSON extended meta-data.

    Parameters
    ----------
    input_image: Union[numpy.ndarray, list[numpy.ndarray]]
        The image data to write, should be a 1-3 dimension ``numpy.ndarray`` 
        or a list of 2-dimensional ``numpy.ndarray``s.
    meta: dict
         will be serialized by JSON and written into the extended header. Note 
         that ``rapidjson`` (the default) or ``json`` (the fallback) cannot 
         serialize all Python objects, so sanitizing ``meta`` to remove non-standard
         library data structures is advisable, including ``numpy.ndarray`` values.
    dtype: Union[numpy.dtype, str]
        will cast the data before writing it.
    pixelsize: Tuple[x,y,z]
        is [z,y,x] pixel size (singleton values are ok for square/cubic pixels)
    pixelunits: str = u'\\AA'
        one of 
        - ``'\\AA'`` for Angstroms
        - ``'pm'`` for picometers
        - ``'\mum'`` for micrometers
        - ``'nm'`` for nanometers. 
        MRC standard is always Angstroms, so pixelsize is converted internally 
        from nm to Angstroms as needed.
    shape: Optional[Tuple[int]]
        is only used if you want to later append to the file, such as 
        merging together Relion particles for Frealign.  Not recommended and 
        only present for legacy reasons.
    voltage: float = 300.0
        accelerating potential in keV
    C3: float = 2.7
        spherical aberration in mm
    gain: float = 1.0
        detector gain in units (counts/primary electron)
    compressor: str = None
        is a choice of ``None, 'lz4', 'zlib', 'zstd'``, plus ``'blosclz'``, ``'lz4hc'`` 
        - ``'lz4'`` is  generally the fastest.
        - ``'zstd'`` generally gives the best compression performance, and is still almost 
          as fast as 'lz4' with ``clevel == 1``.
    clevel: int = 1
        the compression level, 1 is fastest, 9 is slowest. The compression ratio 
        will rise slowly with clevel (but not as fast as the write time slows 
        down).
    n_threads: int = None
        number of threads to use for blosc compression.  Defaults to number of 
        virtual cores if ``== None``.
    quickStats: bool = True
        estimates the image mean, min, max from the first frame only, which 
        saves computational time for image stacks. Generally strongly advised to 
        be ``True``.
    idx 
        can be used to write an image or set of images starting at a specific 
        position in the MRC file (which may already exist). Index of first image 
        is 0. A negative index can be used to count backwards. If omitted, will 
        write whole stack to file. If writing to an existing file, compression 
        or extended MRC2014 headers are currently not supported with this option.

    Returns
    -------
    ``None``

    Warning
    -------
    MRC definitions are not consistent. Generally we support the CCPEM2014 schema
    as much as possible.
    '''

    if not BLOSC_PRESENT and compressor is not None:
        raise ImportError('`blosc` is not installed, cannot use file compression.')

    # For dask, we don't want to import dask, but we can still work-around how to 
    # check its type without isinstance()
    image_type = type(input_image)
    if image_type.__module__ == 'dask.array.core' and image_type.__name__ == 'Array':
        # Ideally it would be faster to iterate over the chunks and pass each one 
        # to blosc but that likely requires c-blosc2
        input_image = input_image.__array__()
        dims = input_image.shape

    slices = 0
    global WARNED_ABOUT_CASTING_F64, WARNED_ABOUT_CASTING_C128

    if isinstance(input_image, (tuple,list)):
        shape = input_image[0].shape
        ndim = input_image[0].ndim
        if ndim == 3:
            slices = shape[0]
            shape = shape[1:]
        elif ndim == 2:
            slices = 1
        else:
            raise ValueError('For a sequence of arrays, only 2D or 3D arrays are handled.')

        dims = np.array([len(input_image)*slices, shape[0], shape[1]])

        # Verify that each image in the list is the same 2D shape and dtype
        first_shape = input_image[0].shape
        first_dtype = input_image[0].dtype
        
        # Cast float64 -> float32, and complex128 -> complex64
        for J, z_slice in enumerate(input_image):
            assert(np.all(z_slice.shape == first_shape))

            if z_slice.dtype == np.float64 or z_slice.dtype == float:
                if not WARNED_ABOUT_CASTING_F64:
                    logger.warn('Casting {} to `numpy.float32`, further warnings will be suppressed.'.format(MRCfilename))
                    WARNED_ABOUT_CASTING_F64 = True
                input_image[J] = z_slice.astype(np.float32)
            elif z_slice.dtype == np.complex128:
                if not WARNED_ABOUT_CASTING_C128:
                    logger.warn('Casting {} to `numpy.complex64`, further warnings will be suppressed.'.format(MRCfilename))
                    WARNED_ABOUT_CASTING_C128 = True
                input_image[J] = z_slice.astype(np.complex64)
            else:
                assert(z_slice.dtype == input_image[0].dtype)


    else: # Array-'like' object
        dims = input_image.shape
        if input_image.ndim == 2:
            # If it's a 2D image we force it to 3D - this makes life easier later:
            input_image = input_image.reshape((1, input_image.shape[0], input_image.shape[1]))

        # Cast float64 -> float32, and complex128 -> complex64
        if input_image.dtype == np.float64 or input_image.dtype == float:
            if not WARNED_ABOUT_CASTING_F64:
                logger.warn('Casting {} to `numpy.float64`'.format(MRCfilename))
                WARNED_ABOUT_CASTING_F64 = True
            input_image = input_image.astype(np.float32)
        elif input_image.dtype == np.complex128:
            if not WARNED_ABOUT_CASTING_C128:
                logger.warn('Casting {} to `numpy.complex64`'.format(MRCfilename))
                WARNED_ABOUT_CASTING_C128 = True
            input_image = input_image.astype(np.complex64)

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
            raise TypeError( 'uint4 packing is not compatible with compression, use int8 datatype.' )
            
        header = {'meta': meta }
        if dtype == None:
            if slices > 0:
                header['dtype'] = endchar + input_image[0].dtype.descr[0][1].strip('<>|')
            else:
                header['dtype'] = endchar + input_image.dtype.descr[0][1].strip('<>|')
        else:
            header['dtype'] = dtype
            
        # Now we need to filter dtype to make sure it's actually acceptable to MRC
        if not header['dtype'].strip('<>|') in REVERSE_CCPEM_ENUM:
            raise TypeError('ioMRC.MRCExport: Unsupported dtype cast for MRC %s' % header['dtype'])
            
        header['dimensions'] = dims
        
        header['pixelsize'] = pixelsize
        header['pixelunits'] = pixelunits
        header['shape'] = shape
        
        # This overhead calculation is annoying but many 3rd party tools that use 
        # MRC require these statistical parameters.
        if bool(quickStats):
            if slices > 0:
                first_image = input_image[0]
            else:
                first_image = input_image[0,:,:]

            imMin = first_image.real.min(); imMax = first_image.real.max()
            header['maxImage'] = imMax
            header['minImage'] =  imMin
            header['meanImage'] = 0.5*(imMax + imMin)
        else:
            if slices > 0:
                header['maxImage'] = np.max( [z_slice.real.max() for z_slice in input_image] )
                header['minImage'] = np.min( [z_slice.real.min() for z_slice in input_image] )
                header['meanImage'] = np.mean( [z_slice.real.mean() for z_slice in input_image] )
            else:
                header['maxImage'] = input_image.real.max()
                header['minImage'] = input_image.real.min()
                header['meanImage'] = input_image.real.mean()
        
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
        if n_threads == None and BLOSC_PRESENT:
            n_threads = DEFAULT_N_THREADS
        header['n_threads'] = n_threads
        
        if dtype == 'uint4':
            if slices > 0:
                raise NotImplementedError('Saving of lists of arrays not supported for `dtype=uint4`')
            # Decimate to packed 4-bit
            input_image = input_image.astype('uint8')
            input_image = input_image[:,:,::2] + np.left_shift(input_image[:,:,1::2],4)

    else: # We are going to append to an already existing file:
        # So we try to figure out its header with 'CCPEM' or 'eman2' file conventions:
        try:
            header, slices = readMRCHeader(MRCfilename, slices=None, endian=endian, fileConvention='CCPEM', pixelunits=pixelunits)

        except ValueError:
            try:
                header, slices = readMRCHeader(MRCfilename, slices=None, endian=endian, fileConvention='eman2', pixelunits=pixelunits)
            except ValueError:
            # If neither 'CCPEM' nor 'eman2' formats satisfy:
                raise ValueError('Error: unrecognized MRC type for file: %s ' % MRCfilename)

        # If the file already exists, its X,Y dimensions must be consistent with the current image to be written:
        if np.any( header['dimensions'][1:] != input_image.shape[1:]):
            raise ValueError('Error: x,y dimensions of image do not match that of MRC file: %s ' % MRCfilename)
            # TO DO: check also consistency of dtype?

        if 'meta' not in header.keys():
            header['meta'] = meta

    # Now that we have a proper header, we go into the details of writing to a specific position:
    if idx != None:
        if header['compressor'] != None:
            raise RuntimeError('Writing at arbitrary positions not supported for compressed files. Compressor = %s' % header['compressor'])

        idx = int(idx)
        # Force 2D to 3D dimensions:
        if len( header['dimensions'] ) == 2:
            header['dimensions'] = np.array([1, header['dimensions'][0], header['dimensions'][1]])

        # Convert negative index to equivalent positive index:
        if idx < 0:
            idx = header['dimensions'][0] + idx

        # Just check if the desired image is within the stack range:
        # In principle we could write to a position beyond the limits of the file (missing slots would be filled with zeros), but let's avoid that the user writes a big file with zeros by mistake. So only positions within or immediately consecutive to the stack are allowed:
        if idx < 0 or idx > header['dimensions'][0]:
            raise ValueError( 'Error: image or slice index out of range. idx = %d, z_dimension = %d' % (idx, header['dimensions'][0]) )

        # The new Z dimension may be larger than that of the existing file, or even of the new file, if an index larger than the current stack is specified:
        newZ = idx + input_image.shape[0]
        if newZ > header['dimensions'][0]:
            header['dimensions'] = np.array([idx + input_image.shape[0], header['dimensions'][1], header['dimensions'][2]])

        # This offset will be applied to f.seek():
        offset = idx * np.product(header['dimensions'][1:]) * np.dtype(header['dtype']).itemsize

    else:
        offset = 0

        
    __MRCExport(input_image, header, MRCfilename, slices, 
                endchar=endchar, offset=offset, idxnewfile=idxnewfile)
 
        
def __MRCExport(input_image, header, MRCfilename, slices, 
                endchar='<', offset=0, idxnewfile=True):
    '''
    MRCExport private interface with a dictionary rather than a mess of function 
    arguments.
    '''

    if idxnewfile: # If forcing a new file we truncate it even if it already exists:
        fmode = 'wb'
    else: # Otherwise we'll just update its header and append images as required:
        fmode = 'rb+'

    with open(MRCfilename, fmode, buffering=BUFFERSIZE) as f:
        extendedBytes = writeMRCHeader(f, header, slices, endchar=endchar)
        f.seek(DEFAULT_HEADER_LEN + extendedBytes + offset)

        dtype = header['dtype']
        if ('compressor' in header) \
                and (header['compressor'] in REVERSE_COMPRESSOR_ENUM) \
                and (REVERSE_COMPRESSOR_ENUM[header['compressor']]) > 0:
            # compressed MRCZ
            logger.debug('Compressing %s with compressor %s%d' %
                    (MRCfilename, header['compressor'], header['clevel']))
            
            applyCast = False
            if slices > 0:
                chunkSize = input_image[0].size
                typeSize = input_image[0].dtype.itemsize
                if dtype != 'uint4' and input_image[0].dtype != dtype: 
                    applyCast = True
            else:
                chunkSize = input_image[0,:,:].size
                typeSize = input_image.dtype.itemsize
                if dtype != 'uint4' and input_image.dtype != dtype: 
                    applyCast = True
                
            blosc.set_nthreads(header['n_threads'])
            # for small image dimensions we need to scale blocksize appropriately
            # so we use the available cores
            block_size = np.minimum(BLOSC_BLOCK, chunkSize//header['n_threads'])
            blosc.set_blocksize(block_size)
            
            header['packedBytes'] = 0
            
            clevel = header['clevel']
            cname = header['compressor']

            # For 3D frames in lists, we need to further sub-divide each frame 
            # into slices so that each channel is compressed seperately by 
            # blosc.
            if slices > 1:
                deep_image = input_image # grab a reference
                input_image = []
                for frame in deep_image:
                    for I in range(slices):
                        input_image.append(frame[I,:,:])

            for J, frame in enumerate(input_image):
                if applyCast:
                    frame = frame.astype(dtype)

                if frame.flags['C_CONTIGUOUS'] and frame.flags['ALIGNED']:
                    # Use pointer
                    compressedData = blosc.compress_ptr(frame.__array_interface__['data'][0], 
                                    frame.size,
                                    typeSize, 
                                    clevel=header['clevel'], 
                                    shuffle=blosc.BITSHUFFLE,
                                    cname=header['compressor'])
                else: 
                    # Use tobytes, which is slower in benchmarking
                    compressedData = blosc.compress(frame.tobytes(),
                                    typeSize, 
                                    clevel=clevel, 
                                    shuffle=blosc.BITSHUFFLE,
                                    cname=cname)
        

                f.write(compressedData)
                header['packedBytes'] += len(compressedData)

            # Rewind and write out the total compressed size
            f.seek(144)
            np.int64(header['packedBytes']).astype(endchar + 'i8').tofile(f)
            
        else: # vanilla MRC
            if slices > 0: 
                if dtype != 'uint4' and dtype != input_image[0].dtype:
                    for z_slice in input_image:
                        z_slice.astype(dtype).tofile(f)
                else:
                    for z_slice in input_image:
                        z_slice.tofile(f)
            else:
                if dtype != 'uint4' and dtype != input_image.dtype:
                    input_image = input_image.astype(dtype)
                input_image.tofile(f)
            
            
    return 
    
def writeMRCHeader(f, header, slices, endchar='<'):
    '''
    Parameters
    ----------
    Writes a  header to the file-like object ``f``, requires a dict 
    called ``header`` to parse the appropriate fields.

    Returns
    -------
    ``None``

    Note
    ----
    Use `defaultHeader()` to retrieve an example with all potential fields.
    '''
    dtype_f4 = endchar + 'f4'
    dtype_i4 = endchar + 'i4'
        
    f.seek(0)
    # Write dimensions
    if len(header['dimensions']) == 2: # force to 3-D
        dimensions = np.array([1, header['dimensions'][0], header['dimensions'][1]])
    else: 
        dimensions = np.array(header['dimensions'])
        
    # Flip to Fortran order
    dimensions = np.flipud(dimensions)
    dimensions.astype(dtype_i4).tofile(f)
    
    # 64-bit floats are automatically down-cast
    dtype = header['dtype'].lower().strip( '<>|' )
    try:
        MRCmode = np.int32(REVERSE_CCPEM_ENUM[dtype]).astype(endchar + 'i4')
    except:
        raise ValueError('Warning: Unknown dtype for MRC encountered = ' + str(dtype))
        
    # Add 1000 * COMPRESSOR_ENUM to the dtype for compressed data
    if ('compressor' in header 
                and header['compressor'] in REVERSE_COMPRESSOR_ENUM 
                and REVERSE_COMPRESSOR_ENUM[header['compressor']] > 0):
        header['compressor'] = header['compressor'].lower()
        MRCmode += MRC_COMP_RATIO * REVERSE_COMPRESSOR_ENUM[header['compressor']]
        
        # How many bytes in an MRCZ file, so that the file can be appended-to.
        try:
            f.seek(144)
            np.int32( header['packedBytes'] ).astype(endchar + 'i8').tofile(f)
        except:
            # This is written afterward so we don't try to keep the entire compressed file in RAM
            pass
            
    f.seek(12)
    MRCmode.tofile(f)
    
    # Print NXSTART, NYSTART, NZSTART
    np.array([0, 0, 0], dtype=dtype_i4).tofile(f)

    # Print MX, MY, MZ, the sampling. We only allow for slicing along the z-axis,
    # e.g. for multi-channel STEM.
    f.seek(36)

    np.int32(slices).astype(dtype_i4).tofile(f)

    # Print cellsize = pixelsize * dimensions
    # '\AA' will eventually be deprecated (probably in Python 3.7/8), please cease using it.
    if header['pixelunits'] == '\\AA' or header['pixelunits'] == '\AA':
        AApixelsize = np.array(header['pixelsize'])
    elif header['pixelunits'] == '\mum':
        AApixelsize = np.array(header['pixelsize'])*10000.0
    elif header['pixelunits'] == 'pm':
        AApixelsize = np.array(header['pixelsize'])/100.0
    else: # Default is nm
        AApixelsize = np.array(header['pixelsize'])*10.0   
    
    # The above AApixelsize insures we cannot have an array of len=1 here
    if isinstance( AApixelsize, np.ndarray ) and AApixelsize.size == 1:
        cellsize = np.array([AApixelsize,AApixelsize,AApixelsize]) * dimensions
    elif not isinstance( AApixelsize, (list,tuple,np.ndarray) ):
        cellsize = np.array([AApixelsize,AApixelsize,AApixelsize]) * dimensions
    elif len(AApixelsize) == 2:
        # Default to z-axis pixelsize of 10.0 Angstroms
        cellsize = np.flipud(np.array( [10.0, AApixelsize[0], AApixelsize[1]])) * dimensions
    else:
        cellsize = np.flipud(np.array( AApixelsize )) *  dimensions
        
    f.seek(40)
    np.array(cellsize, dtype=dtype_f4).tofile(f)
    # Print default cell angles
    np.array([90.0,90.0,90.0], dtype=dtype_f4).tofile(f)
    
    # Print axis associations (we use C ordering internally in all Python code)
    np.array([1,2,3], dtype=dtype_i4).tofile(f)
    
    # Print statistics (if available)
    f.seek(76)
    if 'minImage' in header:
        np.float32(header['minImage']).astype(dtype_f4).tofile(f)
    else:
        np.float32(0.0).astype(dtype_f4).tofile(f)
    if 'maxImage' in header:
        np.float32(header['maxImage']).astype(dtype_f4).tofile(f)
    else:
        np.float32(1.0).astype(dtype_f4).tofile(f)
    if 'meanImage' in header:
        np.float32(header['meanImage']).astype(dtype_f4).tofile(f)
    else:
        np.float32(0.0).astype(dtype_f4).tofile(f)
        
    # We'll put the compressor info and number of compressed bytes in 132-204
    # and new metadata
    # RESERVED: 132: 136: 140 : 144 for voltage, C3, and gain
    f.seek(132)
    if 'voltage' in header:
        np.float32(header['voltage']).astype(dtype_f4).tofile(f)
    if 'C3' in header:
         np.float32(header['C3']).astype(dtype_f4).tofile(f)
    if 'gain' in header:
        np.float32(header['gain']).astype(dtype_f4).tofile(f)
        

    # Magic MAP_ indicator that tells us this is in-fact an MRC file
    f.seek(208)
    f.write(b'MAP ')
    # Write a machine stamp, '17,17' for big-endian or '68,65' for little
    # Note that the MRC format doesn't indicate the endianness of the endian 
    # identifier...
    f.seek(212)
    if endchar == '<':
        f.write(struct.pack(b'BB', 68, 65))
    else:
        f.write(struct.pack(b'BB', 17, 17))

    # Write b'MRCZ<version>' into labels
    f.seek(220)
    f.write(struct.pack(b'i', 1)) # We have one label
    f.write(b'MRCZ' + __version__.encode('ascii'))
    
    # Extended header, if meta is not None
    if isinstance(header['meta'], dict):
        jsonMeta = json.dumps(header['meta'], default=_defaultMetaSerialize).encode('utf-8')

        jsonLen = len(jsonMeta)
        # Length of extended header
        f.seek(92)
        f.write(struct.pack(endchar+'i', jsonLen))

        # 4-byte char ID string of extended metadata type
        f.seek(104)
        f.write(b'json')

        # Go to the extended header
        f.seek(DEFAULT_HEADER_LEN)
        f.write( jsonMeta )
        return jsonLen
    
    # No extended header
    return 0

def asyncReadMRC(*args, **kwargs):
    '''
    Calls `readMRC` in a separate thread and executes it in the background.

    Parameters
    ----------
    Valid arguments are as for `readMRC()`. 

    Returns
    -------
    future
        A ``concurrent.futures.Future()`` object.  Calling ``future.result()`` 
        will halt until the read is finished and returns the image and meta-data
        as per a normal call to `readMRC`.  

    Example
    -------

        worker = asyncReadMRC( 'someones_file.mrc' )
        # Do some work
        mrcImage, mrcMeta = worker.result()
    '''
    return _asyncExecutor.submit(readMRC, *args, **kwargs)

def asyncWriteMRC(*args, **kwargs):
    '''
    Calls `writeMRC` in a seperate thread and executes it in the background.

    Parameters
    ----------
    Valid arguments are as for `writeMRC()`.

    Returns
    -------
    future
        A ``concurrent.futures.Future`` object.  If needed, you can call 
        ``future.result()`` to wait for the write to finish, or check with 
        ``future.done()``. Most of the time you can ignore the return and let 
        the system write unmonitored.  An exception would be if you need to pass 
        in the output to a subprocess.

    Example
    -------

        worker = asyncWriteMRC( npImageData, 'my_mrcfile.mrc' )
        # Do some work
        if not worker.done():
            time.sleep(0.001)
        # File is written to disk
    '''
    return _asyncExecutor.submit(writeMRC, *args, **kwargs)