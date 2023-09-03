import numpy as np

def arrangePattern(diffractionPatterns,N_scanX,N_scanY):
    N_scanx = int(N_scanX)
    N_scany = int(N_scanY)
    outputDatacube=np.zeros(
        (N_scanY,N_scanX,np.shape(diffractionPatterns)[1],np.shape(diffractionPatterns)[2]),
        dtype=diffractionPatterns.dtype
    )
    frameIndex = 0
    for y_index in range(N_scanY):
        for x_index in range(N_scanX):
            outputDatacube[y_index,x_index,:,:] = np.copy(diffractionPatterns[frameIndex,:,:])
            frameIndex = frameIndex+1

    return outputDatacube

#####################################################################################

def applyFlatfieldCorrection(inputFrames,brightfield,darkfield):
    outputFrames = np.zeros(np.shape(inputFrames) ,dtype=inputFrames.dtype)
    
    deadPixelMask = ( ( np.mean(darkfield)*4 ) > darkfield )
    brightfield = np.copy(brightfield*deadPixelMask)
    
    #flatfieldGainMask = np.copy(flatfieldGain)
    flatfieldGainMask = (brightfield-np.amin(brightfield))/np.amax(brightfield-np.amin(brightfield))
    if( (flatfieldGainMask.any()<0) or (flatfieldGainMask.any()>1.0) ):
        print('Error')
    
    for k in range(np.shape(inputFrames)[0]):
        outputFrames[k,:,:] = np.round(inputFrames[k,:,:] * deadPixelMask * flatfieldGainMask)
        
    return outputFrames, flatfieldGainMask, deadPixelMask

#####################################################################################

def importFlatfieldFiles(dateString,datasetFolder="/em_data/berk/AllData/Calibrations/ELA/"):
    flatfieldGain = np.load(datasetFolder+"gain_"+dateString+".npy")
    darkFieldFrame = np.load(datasetFolder+"darkField_"+dateString+".npy")
    
    return flatfieldGain, darkFieldFrame

#####################################################################################

def createLowpassAperture(maxDetailPeriod,pixelSize,Nvals):
    from scipy.fft import fft2, ifft2, fftshift, ifftshift
    dx = pixelSize
    dy = pixelSize

    Fx_Max = 1/(2*dx)
    Fy_Max = 1/(2*dy)

    Fx_linear = np.linspace(-np.floor(Fx_Max),np.ceil(Fx_Max),Nvals[0])
    Fy_linear = np.linspace(-np.floor(Fy_Max),np.ceil(Fy_Max),Nvals[1])

    Fxx,Fyy = np.meshgrid(Fx_linear,Fy_linear)
    
    lowpassFilterAperture = Fxx**2 + Fyy**2 < ((1/maxDetailPeriod))**2
    
    return lowpassFilterAperture, Fx_Max, Fy_Max

#####################################################################################

def createGrating(gratingPeriod,Nvals,Lvals,gratingPhaseDegrees=0.0,directionDegrees=0.0):
    Nx = int(Nvals[0])
    Ny = int(Nvals[1])
    Lx = Lvals[0]
    Ly = Lvals[1]
    dx = Lx/Nx
    dy = Ly/Ny
    
    phaseField = np.ones((Nx,Ny),dtype=complex)*gratingPhaseDegrees*np.pi/180
    
    x_linear = np.arange(-np.floor(Nx/2),np.ceil(Nx/2))
    y_linear = np.arange(-np.floor(Ny/2),np.ceil(Ny/2))
    
    xx_,yy_ = np.meshgrid(x_linear,y_linear)
    xx = np.copy(xx_*dx)
    yy = np.copy(yy_*dy)
    
    directionDegrees_round = np.round(directionDegrees,decimals=2)
    print('Creating a grating towards '+str(directionDegrees_round)+' degrees')
    directionDegrees_round = -directionDegrees_round
    
    u_flat = 1/gratingPeriod
    u_x = u_flat * np.cos(directionDegrees_round * np.pi/180.0)
    u_y = u_flat * np.sin(directionDegrees_round * np.pi/180.0)
    
    output_grating = 0.5+0.5*np.cos(2*np.pi*(xx*u_x + yy*u_y)+phaseField)
        
    return output_grating

#####################################################################################

def createAperture(apertureSize,Nvals,Lvals,shifts=(0,0),apertureType='circular'):
    
    Nx = Nvals[0]
    Ny = Nvals[1]
    Lx = Lvals[0]
    Ly = Lvals[1]
    dx = Lx/Nx
    dy = Ly/Ny
    
    x_linear = np.arange(-np.floor(Nx/2),np.ceil(Nx/2))
    y_linear = np.arange(-np.floor(Ny/2),np.ceil(Ny/2))
    
    xx_,yy_ = np.meshgrid(x_linear,y_linear)
    xx = np.copy(xx_*dx)
    yy = np.copy(yy_*dy)
    
    aperture = np.zeros((Nx,Ny))
    
    x_shift = shifts[0]         # [m]
    y_shift = shifts[1]         # [m]
    
    if(apertureType == 'circular'):
        apertureDiameter = apertureSize # [m]
        aperture[(xx-x_shift)**2+(yy-y_shift)**2 <= (apertureDiameter/2)**2]=1
    elif(apertureType == 'rectangular'):
        aperture = np.zeros((Nx,Ny))
        width_x  = apertureSize[0]  # [m]
        width_y  = apertureSize[1]  # [m]
        aperture[np.logical_and(np.abs(xx-x_shift)<=width_x/2,  np.abs(yy-y_shift)<=width_y/2)]=1
    else:
        aperture = 0
        print('Please choose a valid aperture type!')
        print(' ')
        
    return aperture

#####################################################################################

def exportMRC(inputArray,saveLocationFileName,normalize=False,pixelSize=None,defocusValue=None,stepSize=None):
    import mrcfile
    
    if(len(np.shape(inputArray))==2):
        dx = pixelSize # [m]
        dy = pixelSize # [m]
        Nx = np.shape(inputArray)[1]
        Ny = np.shape(inputArray)[0]
        if(normalize==True):
            inputArray = inputArray.astype(np.float32)
            inputArray = np.copy((inputArray - np.min(inputArray)) / (np.max(inputArray) - np.min(inputArray)))
        
        with mrcfile.new(saveLocationFileName, overwrite=True) as mrc:
            mrc.set_data((inputArray[:,:]).astype(np.float32))
            if(pixelSize is not None):
                mrc.voxel_size = (dx*1e10,dy*1e10,0.0) # MRC uses voxel size in Angstroms
            mrc.update_header_from_data()
            mrc.update_header_stats()
            
#             if(defocusValue is not None):
#                 mrc.header['C1'] = str(int(defocusValue*1e9)) + ' [nm]'

#             if(stepSize is not None):
#                 mrc.header['stepsize'] = str(int(stepSize*1e10)) + ' [A]'
    else:
        print('Something wrong with the array dimensions')
        
#####################################################################################
        
def TFS_to_abTEM_AberrationTranslator(TFS_AberrationList,mode='SI'):
    """
    'C10' = 'C1' 'Defocus'
    'C12' = 'A1' 'Twofold astigmatism' 'Condenser Astigmatism'

    'C21' = 'B2' 'Axial Coma' 'BeamTilt'
    'C23' = 'A2' 'Threefold astigmatism'

    'C30' = 'C3' 'Spherical aberration'
    'C32' = 'S3' 'Axial star aberration'
    'C34' = 'A3' 'Fourfold astigmatism'

    'C45' = 'A4' 'Fivefold astigmatism'
    """
    
    TFS_Keys = ("C1", "A1", "B2", "A2", "C3", "S3", "A3", "A4")
    abTEM_Keys = ("C10", "C12", "C21", "C23", "C30", "C32", "C34", "C45")
    
    abTEM_AberrationList = {}
    
    if(mode=='SI'):
        multiplier = -1e10 # abTEM works in Angstroms
    elif(mode=='angstrom'):
        multiplier = -1 # and also in negative signs
    else:
        print('Please enter a mode for dimensions.')
        multiplier = 0
    
    for keyIndex in range(len(TFS_Keys)):
        currentKey = TFS_Keys[keyIndex]
        if currentKey in TFS_AberrationList:
            abTEM_AberrationList[abTEM_Keys[keyIndex]] = TFS_AberrationList[currentKey]*multiplier
            
    return abTEM_AberrationList

#####################################################################################

def resample_frames_gpu(inputFrames,resampling_factor,GPU_ID=0):
    import cupy as cp
    from cupyx.scipy import ndimage
    with cp.cuda.Device(GPU_ID):
        resampledFrames = cp.asnumpy(ndimage.zoom(cp.asarray(inputFrames), (1,resampling_factor,resampling_factor) ) )
        
    return resampledFrames
        
        
#####################################################################################

def rescale_for_image(inputFrame,outputDataType='uint8'):
    image_min = np.min(saveObject)
    image_max = np.max(saveObject)
    image_range = image_max - image_min
    if(outputDataType=='uint8'):
        image_scaled = ( ((saveObject - image_min) / image_range) * 255 ).astype(np.uint8)
    else:
        print('Not yet implemented!')
    return image_scaled

#####################################################################################

def plot_All_In_Folder(folder_path = 'path/to/folder' , extension = '.jpg' ):

    from PIL import Image
    import matplotlib.pyplot as plt
    import os

    image_files = os.listdir(folder_path)

    # Iterate over the image files in the folder
    for file in image_files:
        if file.endswith(extension):
            image_path = os.path.join(folder_path, file)

            # Load the image using PIL
            image = Image.open(image_path)

            # Visualize the image using matplotlib
            plt.imshow(image)
            plt.title(file)  # Use the file name as the title
            plt.axis('off')  # Turn off axis labels
            plt.show()

            
#####################################################################################

def zoomImage(inputImage,zoom_factor=2):
    # Determine the dimensions of the image
    height, width = inputImage.shape
    
    # Define the region of interest (ROI) for cropping the image
    roi_width = width // zoom_factor
    roi_height = height // zoom_factor
    roi_center = (width // 2, height // 2)
    roi_x = slice(roi_center[0] - roi_width // 2, roi_center[0] + roi_width // 2)
    roi_y = slice(roi_center[1] - roi_height // 2, roi_center[1] + roi_height // 2)
    return inputImage[roi_y, roi_x]
            
#####################################################################################

def cropDownTo(inputImage, crop_size = (1024,1024)):
    height, width = inputImage.shape[:2]
    left = (width - crop_size[0]) // 2
    top = (height - crop_size[1]) // 2
    right = left + crop_size[0]
    bottom = top + crop_size[1]
    return inputImage[top:bottom, left:right]

#####################################################################################

def bin2D(array, factor, dtype=np.float64):
    """
    Bin a 2D ndarray by binfactor.

    Args:
        array (2D numpy array):
        factor (int): the binning factor
        dtype (numpy dtype): datatype for binned array. default is numpy default for
            np.zeros()

    Returns:
        the binned array
    """
    x, y = array.shape
    binx, biny = x // factor, y // factor
    xx, yy = binx * factor, biny * factor

    # Make a binned array on the device
    binned_ar = np.zeros((binx, biny), dtype=dtype)
    array = array.astype(dtype)

    # Collect pixel sums into new bins
    for ix in range(factor):
        for iy in range(factor):
            binned_ar += array[0 + ix:xx + ix:factor, 0 + iy:yy + iy:factor]
    return binned_ar

#####################################################################################

def plot_fft(image, threshold, is_lines=False):
    
    import scipy.fft as fft
    import matplotlib.pyplot as plt
    from scipy import ndimage
    
    # Convert the image to grayscale if needed
    if len(image.shape) == 3:
        image = np.mean(image, axis=2)
    
    # Normalize the image
    image = image.astype(float)
    image /= np.max(image)
    
    # Calculate the 2D FFT
    image_fft = fft.fft2(image)
    
    # Shift the zero frequency component to the center
    image_fft_shifted = fft.fftshift(image_fft)
    
    # Calculate the magnitude spectrum
    magnitude_spectrum = np.log(1 + np.abs(image_fft_shifted))
    
    # Perform spectral peak/line detection
    if is_lines:
        peaks, _ = ndimage.label(magnitude_spectrum > threshold)
        labeled_indices = np.array(ndimage.center_of_mass(magnitude_spectrum, labels=peaks, index=np.arange(1, peaks.max() + 1)))
    else:
        peaks, _ = ndimage.label(magnitude_spectrum > threshold)
        peak_indices = np.array(ndimage.center_of_mass(magnitude_spectrum, labels=peaks, index=np.arange(1, peaks.max() + 1)))
    
    # Plot the magnitude spectrum with peaks/lines
    plt.imshow(magnitude_spectrum, cmap='gray')
    if is_lines:
        plt.plot(labeled_indices[:, 1], labeled_indices[:, 0], 'ro', markersize=5, label='Lines')
    else:
        plt.plot(peak_indices[:, 1], peak_indices[:, 0], 'ro', markersize=5, label='Peaks')
    plt.legend()
    plt.show()


         