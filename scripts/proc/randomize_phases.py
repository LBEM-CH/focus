#!/usr/bin/env python
import focus_utilities as util
import ioMRC
import sys
import numpy as np

stackin = ioMRC.readMRC( sys.argv[1] )[0]
stackout = sys.argv[2]
angpix = np.float( sys.argv[3] )
lowpass = np.float( sys.argv[4] )

for i in range( stackin.shape[0] ):

	print( ' Randomizing phases of slice %d/%d beyond %f A...' % ( i+1, stackin.shape[0], lowpass ) )
	ioMRC.writeMRC( util.HighResolutionNoiseSubstitution( stackin[i,:,:], lp=lowpass, apix=angpix ), stackout, idx=i )

print( 'Done!' )