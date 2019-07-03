# Python utilities for CTF correction
# Author: Ricardo Righetto
# E-mail: ricardo.righetto@unibas.ch

import numpy as np
import focus_utilities as util
import copy
import numexpr as ne

pi = np.pi

def CTF(imsize=[100, 100], DF1=1000.0, DF2=None, AST=0.0, WGH=0.10, Cs=2.7, kV=300.0, apix=1.0, B=0.0, rfft=True):
    # Generates 2D CTF function
    # Underfocus is positive following conventions of FREALIGN and most of the packages out there (in Angstroms).
    # B is B-factor
    # rfft is to compute only half of the FFT (i.e. real data) if True, or the full FFT if False.

    # try:

    #     dummy = np.fft.rfftfreq.func_name

    # except AttributeError:

    #     raise AttributeError("""\nERROR: Your version of NumPy does not contain numpy.fft.rfftfreq. Please switch to NumPy version 1.8.0 or later.\nSometimes this error occurs due to the Python environment being overshadowed by another program such as EMAN2, for example.\nIf you are using FOCUS from the GUI you can check this under Settings >> Software.""")

    if not np.isscalar(imsize) and len(imsize) == 1:

        imsize = imsize[0]

    Cs *= 1e7  # Convert Cs to Angstroms

    if DF2 == None or np.isscalar(imsize):

        DF2 = DF1

    # else:

    #   # NOTATION FOR DEFOCUS1, DEFOCUS2, ASTIGMASTISM BELOW IS INVERTED DUE TO NUMPY CONVENTION:
    #   DF1, DF2 = DF2, DF1

    AST *= -pi / 180.0

    WL = ElectronWavelength(kV)

    w1 = np.sqrt(1 - WGH * WGH)
    w2 = WGH

    import warnings
    with warnings.catch_warnings():
        warnings.filterwarnings("ignore", category=RuntimeWarning)

        if np.isscalar(imsize):

            if rfft:
                rmesh = np.fft.rfftfreq(imsize)
            else:
                rmesh = np.fft.fftfreq(imsize)
            amesh = 0.0

        else:

            # rmesh,amesh = focus_utilities.RadialIndices( imsize, RFFT=True )
            xmesh = np.fft.fftfreq(imsize[0])
            if rfft:
                ymesh = np.fft.rfftfreq(imsize[1])
            else:
                ymesh = np.fft.fftfreq(imsize[1])

            xmeshtile = np.tile(xmesh, [len(ymesh), 1]).T
            ymeshtile = np.tile(ymesh, [len(xmesh), 1])

            # rmesh = np.sqrt(xmeshtile * xmeshtile +
            #                 ymeshtile * ymeshtile) / apix
            rmesh = ne.evaluate("sqrt(xmeshtile * xmeshtile + ymeshtile * ymeshtile) / apix")

            amesh = np.nan_to_num(ne.evaluate("arctan2(ymeshtile, xmeshtile)"))

        rmesh2 = ne.evaluate("rmesh * rmesh")

        # From Mindell & Grigorieff, JSB 2003:
        DF = ne.evaluate("0.5 * (DF1 + DF2 + (DF1 - DF2) * cos(2.0 * (amesh - AST)))")

        Xr = np.nan_to_num(ne.evaluate("pi * WL * rmesh2 * (DF - 1.0 / (2.0 * WL * WL * rmesh2 * Cs))"))

    sinXr = ne.evaluate("sin(Xr)")
    cosXr = ne.evaluate("cos(Xr)")
    # CTFreal = w1 * sinXr - w2 * cosXr
    # CTFimag = -w1 * cosXr - w2 * sinXr

    # CTFim = CTFreal + CTFimag*1j
    CTFim = ne.evaluate("-w1 * sinXr - w2 * cosXr")

    if B != 0.0:  # Apply B-factor only if necessary:

        ne.evaluate("CTFim * np.exp(-B * (rmesh2) / 4)", out=CTFim)

    return CTFim


def ElectronWavelength(kV=300.0):
    # Returns electorn wavelength in Angstroms
    # kV *= 1e3 # ensure Kilovolts for below formula
    # return 12.2639 / np.sqrt( kV + 0.97845 * kV*kV / ( 1e6 ) )
    return 12.2639 / np.sqrt(kV * 1e3 + 0.97845 * kV * kV)


def FirstZeroCTF(DF=1000.0, WGH=0.10, Cs=2.7, kV=300.0):
    # Finds the resolution at the first zero of the CTF
    # Wolfram Alpha, solving for -w1 * sinXr - w2 * cosXr = 0
    # https://www.wolframalpha.com/input/?i=solve+%CF%80*L*(g%5E2)*(d-1%2F(2*(L%5E2)*(g%5E2)*C))%3Dn+%CF%80+-+tan%5E(-1)(c%2Fa)+for+g

    Cs *= 1e7  # Convert Cs to Angstroms

    w1 = np.sqrt(1 - WGH * WGH)
    w2 = WGH

    WL = ElectronWavelength(kV)

    g = np.sqrt(-2 * Cs * WL * np.arctan2(w2, w1) + 2 * pi *
                Cs * WL + pi) / (np.sqrt(2 * pi * Cs * DF) * WL)

    return g


def CorrectCTF(img, DF1=1000.0, DF2=None, AST=0.0, WGH=0.10, invert_contrast=False, Cs=2.7, kV=300.0, apix=1.0, phase_flip=False, ctf_multiply=False, wiener_filter=False, C=1.0, return_ctf=False):
    # Applies CTF correction to image
    # Type can be one of the following:
    # 0 - Phase-flipping only
    # 1 - CTF multiplication
    # 2 - Wiener filtering with Wiener constant C
    # By default will return images with same contrast as input, otherwise set invert_contrast=True.

    # Direct CTF correction would invert the image contrast. By default we don't do that, hence the negative sign:
    CTFim = -CTF(img.shape, DF1, DF2, AST, WGH, Cs, kV, apix, 0.0, rfft=True)

    CTFcor = []
    cortype = []

    if invert_contrast:

        ne.evaluate("CTFim * -1.0", out=CTFim)

    FT = np.fft.rfftn(img)

    if phase_flip:  # Phase-flipping

        s = np.sign(CTFim)
        CTFcor.append(np.fft.irfftn(ne.evaluate("FT * s")))
        cortype.append('pf')

    if ctf_multiply:  # CTF multiplication

        CTFcor.append(np.fft.irfftn(ne.evaluate("FT * CTFim")))
        cortype.append('cm')

    if wiener_filter:  # Wiener filtering

        if np.any(C <= 0.0):

            raise ValueError(
                "Error: Wiener filter constant cannot be less than or equal to zero! C = %f " % C)

        CTFcor.append(np.fft.irfftn(ne.evaluate("FT * CTFim / (CTFim * CTFim + C)")))
        cortype.append('wf')

    # else:

    #   raise ValueError( "Error: Type of CTF correction must be 0 (phase-flipping), 1 (CTF multiplication) or 2 (Wiener filtering). ctftype = %d " % ctftype )

    # if return_half:

    #   # AmpHalf = np.zeros( img.shape )
    #   CTFnorm = CTFim**2
    #   AmpHalf = np.abs( FT )**2
    #   AmpHalf = AmpHalf - AmpHalf.mean() + CTFnorm.mean()
    #   AmpHalf = AmpHalf*CTFnorm.std()/AmpHalf.std()
    #   # CTFnorm -= CTFnorm.mean()
    #   # CTFnorm /= CTFnorm.std()
    #   AmpHalf[:,img.shape[1]/2:] = CTFnorm[:,img.shape[1]/2:]
    #   # AmpHalf = AmpHalf**2

    # if return_ctf and return_half:

    #   return CTFcor.real, CTFim, AmpHalf

    # elif return_ctf:

    #   return CTFcor.real, CTFim

    # elif return_half:

    #   return CTFcor.real, AmpHalf

    if return_ctf:

        CTFcor.append(CTFim)

    CTFcor.append(cortype)

    return CTFcor


def AdhocSSNR(imsize=[100, 100], apix=1.0, DF=1000.0, WGH=0.1, Cs=2.7, kV=300.0, S=1.0, F=1.0, hp_frac=0.01, lp=True):
        # Ad hoc SSNR model for micrograph deconvolution as proposed by Dimitry Tegunov. For details see:
        # https://www.biorxiv.org/content/10.1101/338558v1
        # https://github.com/dtegunov/tom_deconv/blob/master/tom_deconv.m

    rmesh = util.RadialIndices(
        imsize, rounding=False, normalize=True, rfft=True)[0]
    ne.evaluate("rmesh / apix", out=rmesh)
    # The ad hoc SSNR exponential falloff
    falloff = ne.evaluate("exp(-100 * rmesh * F) * 10**(3 * S)")

    # The cosine-shaped high-pass filter. It starts at zero frequency and reaches 1.0 at hp_freq (fraction of the Nyquist frequency)
    a = np.minimum(1.0, ne.evaluate("rmesh * apix / hp_frac"))
    highpass = ne.evaluate("1.0 - cos(a * pi/2)")

    if lp:

        # Ensure the filter will reach zero at the first zero of the CTF
        first_zero_res = FirstZeroCTF(DF=DF, WGH=WGH, Cs=Cs, kV=kV)
        a = np.minimum(1.0, ne.evaluate("rmesh / first_zero_res"))
        lowpass = ne.evaluate("cos(a * pi/2)")

        ssnr = ne.evaluate("highpass * falloff * lowpass")  # Composite filter

    else:

        ssnr = ne.evaluate("highpass * falloff")  # Composite filter

    return ssnr
