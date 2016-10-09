# -*- coding: utf-8 -*-
"""
Created on Mon Jun 13 11:38:56 2016

@author: Robert A. McLeod
@email: robert.mcleod@unibas.ch
"""

# from EMAN2 import *
# from sparx import *

import numpy as np
import os, os.path, glob
import subprocess
import sys
try:
    import zorro
except Exception as e:
    print( "Failed to import zorro.  You should used anaconda python, and not have EMAN2 environment variables defined." )
    print( " " ) 
    print( "Likely cause: " )
    print( "   if you called " )
    print( "      source ${proc_2dx}/initialize ")
    print( "   then you need to have before that ")
    print( "      set noEMAN = y" )
    print( " " ) 
    print( e )

####### PARAMETERS FOR ZORRO ######

# n_threads = 16 # number of threads for numexpr and FFTW
# savePNGs = False # Do you want Zorro diagnostic images?
# pixelsize = 1.326 # pixel size in Angstroms
# a = 4.0 # lattice parameter a, Angstroms
# b = 4.0 # lattice parameter b, Angstroms
# gamma = np.pi/4.0 # crystal rotation parammeter
# outputFolder = "./zorro/"
# fileDescriptor = "micrographs/*.dm4" # Can be single file, have wildcards, or be a Python list

# If we want support for the Falcon and DE-20, which are true 4k detectors, we need to change zorroReg.shapePadded too
# zorro.zorro_util.findValidFFTWDim() is useful here.

# Also Fourier cropping can change somewhat, depending on how far out Fourier spots are observed.  
# I don't know if you have an autocorrelation routine to estimate this?


##### Zorro for 2dx Prototype script #####

def get_resolution( gamma, a, b):
    """
    Calculate the largest periodicity from the [hkl] = [1,1], [0,1], [1,0] given gamma, a, b, c
    
    gamma is radians
    (a,b,c) are in Angstroms
    pixelsize is in Angstroms
    
    Returns resolution in inverse Angstroms (must be converted to pixels by Zorro)
    """
    
    astar = 1.0 / (a * np.sin(gamma) )
    bstar = 1.0 / (b * np.sin(gamma) )
    
    # recgamma = np.pi - gamma
    qstar010 = 1.0 / bstar**2.0
    qstar100 = 1.0 / astar**2.0 
    
    return np.min( [qstar010, qstar100] )


print("::Zorro starting...")

print("2dx_zorro_sub.py")

if len(sys.argv) != 17:
                sys.exit("Wrong number of parameters given for 2dx_zorro_sub.py ")

n_threads = int(sys.argv[1])
savePNGs = sys.argv[2]
pixelsize = float(sys.argv[3])/10
a = float(sys.argv[4])/10
b = float(sys.argv[5])/10
gamma = np.deg2rad(float(sys.argv[6]))
outputFolder = sys.argv[7]
fileDescriptor = sys.argv[8]
scratch = sys.argv[9]
KV = float(sys.argv[10])
CS = float(sys.argv[11])
gainfactor = float(sys.argv[12])
stepdigitizer = float(sys.argv[13])
x_dim = int(sys.argv[14])
y_dim = int(sys.argv[15])
z_dim = int(sys.argv[16])

zorroReg = zorro.ImageRegistrator()
zorroReg.pixelsize = pixelsize
zorroReg.maxShift = zorroReg.pixelsize * get_resolution( gamma, a, b )
zorroReg.preShift = False
print( "Estimated distance to first spot in FFT (pix): %f" % zorroReg.maxShift )
zorroReg.plotDict['transparent'] = False
zorroReg.CTFProgram = None
# zorroReg.filterMode = 'hot,dose,background'
zorroReg.filterMode = 'hot,dosenorm,background'
zorroReg.n_threads = n_threads
zorroReg.cachePath = scratch
zorroReg.voltage = KV
zorroReg.C3 = CS
zorroReg.gain = gainfactor
zorroReg.detectorPixelSize = stepdigitizer


if x_dim <= 4096 and y_dim <= 4096:
  zorroReg.shapeBinned = [y_dim,x_dim]

if x_dim == 9999:
  zorroReg.shapeBinned = [3710,3838]

# Option 1: bin it
# zorroReg.shapeBinned = [3838,3710]
# Option 2: pad to 8k
# zorroReg.shapePadded = [8192,8192]

zorroReg.savePNG = True

if isinstance( fileDescriptor, list ) or isinstance( fileDescriptor, tuple ):
    # Do nothing
    fileList = fileDescriptor
elif isinstance( fileDescriptor, str ):
    fileList = glob.glob( fileDescriptor )

print("a = " + str(a))
print("b = " + str(b))
print("gamma in rad = " + str(gamma))
print("FileDescriptor = " + str(fileDescriptor) )


# Normalize the path so we keep everything simple.
for fileName in fileList:
    print("fileName = "+fileName)
    baseName = os.path.basename( fileName )
    baseFront = os.path.splitext( baseName )[0]
    
    # Set individual file names and save a configuration file for each.
    zorroReg.files = {}
    zorroReg.files['figurePath'] = './fig'
    zorroReg.files['config'] = baseName + ".zor"
    zorroReg.files['stack'] = os.path.realpath( fileName )
    print("zorroReg.files['stack'] = " + str(zorroReg.files['stack']))
    zorroReg.files['align'] = baseFront + "_zorro_movie.mrcs"
    zorroReg.files['sum'] = baseFront + "_zorro.mrc"
    
    zorroReg.saveConfig()
    #realConfig = os.path.join( os.path.realpath( outputFolder ), baseName + ".zor" )
    zorro.call( zorroReg.files['config'] )
    
    
