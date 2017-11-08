#!/usr/bin/env python

import focus_utilities as util
import ioMRC
import sys

def main():

	stack = ioMRC.readMRC( sys.argv[1] )[0]
	output = sys.argv[2]
	apix = float( sys.argv[3] )
	frame_dose = float( sys.argv[4] )
	pre_dose = float( sys.argv[5] )
	total_dose = float( sys.argv[6] )
	kv = float( sys.argv[7] )

	dw_avg = util.FilterDoseWeight( stack, apix=apix, frame_dose=frame_dose, pre_dose=pre_dose, total_dose=total_dose, kv=kv )

	ioMRC.writeMRC( dw_avg, output, dtype='float32', pixelsize=apix, quickStats=False )


if __name__ == "__main__":
	main()