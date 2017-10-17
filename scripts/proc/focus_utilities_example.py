import mrcz
# if you install mrcz from pip:
# from mrcz import ioMRC
import focus_utilities as fu
import focus_ctf as ctf
import matplotlib.pyplot as plt
import matplotlib.cm as cmap

# Read the image and its header:
img,hed = ioMRC.readMRC( '/path/to/micrograph0001.mrc' )

# Apply a cosine-edge low-pass filter, half-width at 20.0, 6-Fourier-pixels wide:
# Other filter choices available, such as Gaussian, B-factor, etc. Low-pass and high-pass options can be combined.
img_filt = fu.FilterCosine( img, apix=1.0, lp=20.0, width=6.0, return_filter=False )

# Display the filtered image:
plt.imshow( img_filt, cmap=cmap.gray )
plt.show()
plt.close

# Apply CTF-correction to this micrograph (defocus and astigmatism determined by Gctf):
imgctf,cortype = ctf.CorrectCTF( img, DF1 = 28329.113281, DF2 = 28981.464844, AST = 16.717291, WGH = 0.07, invert_contrast = False, Cs = 2.7, kV = 300.0, apix = 1.0, phase_flip = True )
# Display CTF-corrected micrograph:
plt.imshow( imgctf, cmap=cmap.gray )
plt.show()
plt.close