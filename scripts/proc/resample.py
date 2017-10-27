#!/usr/bin/env python
import focus_utilities as util
import ioMRC
import sys
import numpy as np

stackin = ioMRC.readMRC( sys.argv[1] )[0]
stackout = sys.argv[2]
factor = np.float( sys.argv[3] )
newsize = np.round( np.array( stackin.shape[1:] ) * factor  )

for i in range( stackin.shape[0] ):
	print(' Resampling slice %d/%d ' % ( i+1, stackin.shape[0] ) )
	ioMRC.writeMRC( util.Resample( stackin[i,:,:], newsize=newsize ), stackout, idx=i )

print( 'Done!' )