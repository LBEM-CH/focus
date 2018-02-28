#!/usr/bin/env python
# This script assesses the resolution of CTF estimation from Gctf

import numpy as np
import sys
import focus_utilities as util

epa_file = sys.argv[1]
ccc_thr = float( sys.argv[2] )

epa = np.loadtxt( epa_file, skiprows=1 ).astype( 'float32' )

print 1. / util.ResolutionAtThreshold( epa[:,0], epa[:,4], ccc_thr, nyquist_is_fine=True )