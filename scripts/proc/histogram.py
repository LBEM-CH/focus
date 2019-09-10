from EMAN2 import *
from sparx import *

import matplotlib.pyplot as plt
import numpy as np
from matplotlib import colors
from matplotlib.ticker import PercentFormatter
from PIL import Image, ImageDraw, ImageFont, ImageFilter


import os


if __name__ == "__main__":
	
	if len(sys.argv) != 5:
		sys.exit("Missuse detected (histogram)")

	filename_in = sys.argv[1]
	filename_out = sys.argv[2]
	min = float(sys.argv[3])
	max = float(sys.argv[4])
	
	frame = Image.open(filename_in)
	# frame = get_image(filename_in)

	print( "Read image ", filename_in, ",  MIN = ",min,", MAX = ",max )

	n_bins = 20

	# h = histogram(frame, n_bins)

	x = np.array(frame)
	x = x * (max - min)
	x = x / 256
	x += min

	# imgplot = plt.imshow(x)

	plt.hist(x.ravel(), bins=256, range=(min, max), fc='k', ec='k')
	# plt.hist(x.ravel(), bins=256)
	# plt.show()

	# plt.figure(1)
	# plt.subplot(211)

	plt.savefig(filename_out)

