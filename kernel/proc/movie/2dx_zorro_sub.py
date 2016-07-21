# -*- coding: utf-8 -*-
"""
Created on Mon Jun 13 11:38:56 2016

@author: Robert A. McLeod
@email: robert.mcleod@unibas.ch
"""

# from EMAN2 import *
# from sparx import *


import numpy as np
import zorro
import os, os.path, glob
import subprocess
import sys

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

print "2dx_zorro_sub.py"

if len(sys.argv) != 9:
                sys.exit("Usage: 2dx_zorro_sub.py <correct parameters> ")

n_threads = int(sys.argv[1])
savePNGs = sys.argv[2]
pixelsize = float(sys.argv[3])
a = float(sys.argv[4])
b = float(sys.argv[5])
gamma = float(sys.argv[6])
outputFolder = sys.argv[7]
fileDescriptor = sys.argv[8]


zorroReg = zorro.ImageRegistrator()
zorroReg.pixelsize = pixelsize
zorroReg.maxShift = zorroReg.pixelsize * get_resolution( gamma, a, b )
zorroReg.preShift = True
print( "Estimated distance to first spot in FFT (pix): %f" % zorroReg.maxShift )
zorroReg.plotDict['transparent'] = False
zorroReg.CTFProgram = None
zorroReg.filterMode = 'dose,background'
zorroReg.n_threads = n_threads

if isinstance( fileDescriptor, list ) or isinstance( fileDescriptor, tuple ):
    # Do nothing
    fileList = fileDescriptor
elif isinstance( fileDescriptor, str ):
    fileList = glob.glob( fileDescriptor )

# Normalize the path so we keep everything simple.
for fileName in fileList:
    baseName = os.path.basename( fileName )
    baseFront = os.path.splitext( baseName )
    
    # Set individual file names and save a configuration file for each.
    zorroReg.files = {}
    zorroReg.files['figurePath'] = './fig'
    zorroReg.files['config'] = baseName + ".log"
    zorroReg.files['stack'] = os.path.realpath( fileName )
    zorroReg.files['align'] = baseFront + "_zorro_movie.mrcs"
    zorroReg.files['sum'] = baseFront + "_zorro.mrc"
    
    realConfig = os.path.join( os.path.realpath( outputFolder ), baseName + ".log" )
    zorro.call( realConfig )
    
    
