February 23, 2012
=================

An additional parameter, dAst, was added to CARD 4 to restrain the amount of astigmatism in the CTF fit. This makes the fitting procedure more robust, especially in cases where the Thon rings are not easily visible.


July 25, 2010
=============

ctffind3 (version 3.4 and higher) and ctftilt (version 1.5 and higher) can now make use of openMP. Use the _mp makefiles to compile, for example

make -f Makefile_linux_mp

This will produce executables

ctffind3_mp.exe
ctftilt_mp.exe

To use more than one processor, you will need to set the environmental variable NCPUS. For example, when using the csh shell:

setenv NCPUS 8

to use 8 processors. With 8 processors, ctffind3 runs about 6 times faster and ctftilt about 4 times faster.


Software description
====================

This software is distributed under the GNU General Public License (GPL).

If you use this software, please cite

Mindell, J. A. & Grigorieff, N. (2003) Accurate determination of local defocus and specimen tilt in electron microscopy. J. Struct. Biol. 142, 334-347.


C*****************************************************************************
C
C       CTFFIND3 - determines defocus and astigmatism for images of
C       arbitrary size (MRC format). Astigmatic angle is measured form
C       x axis (same convetiones as in the MRC 2D image processing
C       programs).
C
C       CARD 1: Input file name for image
C       CARD 2: Output file name to check result
C       CARD 3: CS[mm], HT[kV], AmpCnst, XMAG, DStep[um]
C       CARD 4: Box, ResMin[A], ResMax[A], dFMin[A], dFMax[A], FStep[A], dAst[A]
C
C               The output image file to check the result of the fitting
C               shows the filtered average power spectrum of the input
C               image in one half, and the fitted CTF (squared) in the
C               other half. The two halfs should agree very well for a
C               sucessfull fit.
C
C               CS: Spherical aberration coefficient of the objective in mm
C               HT: Electron beam voltage in kV
C               AmpCnst: Amount of amplitude contrast (fraction). For ice
C                        images 0.07, for negative stain about 0.15.
C               XMAG: Magnification of original image
C               DStep: Pixel size on scanner in microns
C               Box: Tile size. The program devides the image into square
C                    tiles and calculates the average power spectrum. Tiles
C                    with a significatly higher or lower variance are
C                    excluded; these are parts of the image which are unlikely
C                    to contain useful information (beam edge, film number
C                    etc). IMPORTANT: Box must have even pixel dimensions.
C               ResMin: Low resolution end of data to be fitted.
C               ResMaX: High resolution end of data to be fitted.
C               dFMin: Starting defocus value for grid search in Angstrom.
C                      Positive values represent an underfocus. The program
C                      performs a systematic grid search of defocus values
C                      and astigmatism before fitting a CTF to machine
C                      precision.
C               dFMax: End defocus value for grid search in Angstrom.
C               FStep: Step width for grid search in Angstrom.
C               dAst: Expected amount of astigmatism in Angstrom.
C
C*****************************************************************************
C       example command file (UNIX):
C
C       #!/bin/csh -x
C       #
C       #   ctffind3
C       #
C       time /public/image/bin/ctffind3.exe << eof
C       image.mrc
C       power.mrc
C       2.6,200.0,0.07,60000.0,28.0
C       128,100.0,15.0,30000.0,90000.0,5000.0,500.0,100.0
C       eof
C       #
C*****************************************************************************



C*****************************************************************************
C
C       CTFTILT - determines defocus, astigmatism tilt axis and tilt angle
C       for images of arbitrary size (MRC format). Astigmatic angle is measured
C       from x axis (same conventions as in the MRC 2D image processing
C       programs).
C
C       CARD 1: Input file name for image
C       CARD 2: Output file name to check result
C       CARD 3: CS[mm], HT[kV], AmpCnst, XMAG, DStep[um],PAve
C       CARD 4: Box, ResMin[A], ResMax[A], dFMin[A], dFMax[A], FStep[A], dAst[A], TiltA[deg], TiltR[deg]
C
C               The output image file to check the result of the fitting
C               shows the filtered average power spectrum of the input 
C               image in one half, and the fitted CTF (squared) in the
C               other half. The two halves should agree very well for a
C               successful fit.
C
C               CS: Spherical aberration coefficient of the objective in mm
C               HT: Electron beam voltage in kV
C               AmpCnst: Amount of amplitude contrast (fraction). For ice
C                        images 0.07, for negative stain about 0.15.
C               XMAG: Magnification of original image
C               DStep: Pixel size on scanner in microns
C               PAve: Pixel averaging (PAve x PAve) for input image
C               Box: Tile size. The program divides the image into square
C                    tiles and calculates the average power spectrum. Tiles
C                    with a significantly higher or lower variance are
C                    excluded; these are parts of the image which are unlikely
C                    to contain useful information (beam edge, film number
C                    etc). IMPORTANT: Box must have a even pixel dimensions.
C               ResMin: Low resolution end of data to be fitted.
C               ResMaX: High resolution end of data to be fitted.
C               dFMin: Starting defocus value for grid search in Angstrom.
C                      Positive values represent an underfocus. The program
C                      performs a systematic grid search of defocus values
C                      and astigmatism before fitting a CTF to machine
C                      precision.
C               dFMax: End defocus value for grid search in Angstrom.
C               FStep: Step width for grid search in Angstrom.
C               dAst: Expected amount of astigmatism in Angstrom. 
C               TiltA: Expected tilt angle in degrees.
C               TiltR: Expected tilt angle uncertainty in degrees.
C
C*****************************************************************************
C       example command file (UNIX):
C
C       #!/bin/csh -x
C       #
C       #   ctftilt
C       #
C       time /public/image/bin/ctftilt.exe << eof
C       image.mrc
C       power.mrc
C       2.6,200.0,0.07,60000.0,28.0,2
C       128,100.0,15.0,30000.0,90000.0,5000.0,500.0,100.0,0.0,5.0
C       eof
C       #
C*****************************************************************************
