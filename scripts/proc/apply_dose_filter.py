#!/usr/bin/env python

import focus_utilities as util
from mrcz import ioMRC
import sys
import numpy as np
import numexpr as ne

def main():

	stack = ioMRC.readMRC( sys.argv[1] )[0][0]
	output = sys.argv[2]
	apix = float( sys.argv[3] )
	frame_dose = float( sys.argv[4] )
	frames_thrown = int( sys.argv[5] )
	pre_dose = frames_thrown * frame_dose
	print( "pre_dose = %.6f" % pre_dose )
	num_frames = int( sys.argv[6] )
	total_dose = pre_dose + num_frames * frame_dose
	print( "total_dose = %.6f" % total_dose )
	kv = float( sys.argv[7] )

	if sys.argv[8] == 'y':
		apply_dw = True
	else:
		apply_dw = False

	if apply_dw:

		dw_avg = util.FilterDoseWeight( stack, apix=apix, frame_dose=frame_dose, pre_dose=pre_dose, total_dose=total_dose, kv=kv )

	else:

		# dw_avg = np.sum( stack[:num_frames,:,:], axis=0 )
		s = stack[:num_frames,:,:]
		dw_avg = ne.evaluate("sum(s, axis=0)")


	dw_avg = util.NormalizeImg( dw_avg, mean=0.0, std=100.0 ) # Normalize the new DW-rec image

	ioMRC.writeMRC( dw_avg, output, dtype='float32', pixelsize=apix, quickStats=False )


if __name__ == "__main__":
	main()