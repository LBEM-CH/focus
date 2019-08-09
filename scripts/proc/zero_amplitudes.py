#!/usr/bin/env python
import focus_utilities as util
from mrcz import ioMRC
import sys
import numpy as np

stackin = ioMRC.readMRC( sys.argv[1] )[0][0]
stackout = sys.argv[2]
angpix = np.float( sys.argv[3] )
lowpass = np.float( sys.argv[4] )

for i in range( stackin.shape[0] ):

	print( 'Zeroing amplitudes of slice %d/%d beyond %f A...' % ( i+1, stackin.shape[0], lowpass ) )
	ioMRC.writeMRC( util.FilterCosine( stackin[i,:,:], lp=lowpass, apix=angpix, width=0 ), stackout, idx=i )

print( 'Done!' )