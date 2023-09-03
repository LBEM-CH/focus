import sys
print(sys.prefix)
import numpy as np

import py4DSTEM
print(py4DSTEM.__version__)
from py4DSTEM.process.utils import fourier_resample

import cupy as cp
from matplotlib import pyplot as plt
from scipy import ndimage, datasets
from scipy.fft import fftshift, ifftshift, fft2, ifft2

import nexusformat
import hdf5plugin
from nexusformat.nexus import NXdata, NXentry, NXfield, nxopen

from PIL import Image

from berkHelperFunctions import (arrangePattern, 
                                TFS_to_abTEM_AberrationTranslator, 
                                applyFlatfieldCorrection, 
                                zoomImage, createLowpassAperture,
                                cropDownTo, exportMRC)

###################################################
# User settings

# Note to self: Watch out for croppings of the dataset!

fileIndexArray = np.arange(17,116+1) # np.arange(144,240+1) #np.arange(17,116+1)
GPU_DeviceNum = 0
datasetFolder = "/em_data/berk/AllData/data4DSTEM/2023_07_11/square1"

#fileIndexArray = np.arange(144,240+1) # np.arange(144,240+1) #np.arange(17,116+1)
#GPU_DeviceNum = 1
#datasetFolder = "/em_data/berk/AllData/data4DSTEM/2023_07_11/square2"

saveToFolder = "/em_data/berk/AllData/data4DSTEM/2023_07_11/allImages_potential_variedRot/"

R_pixel = 20.00
SemiConvAngle = 4.0
AccVoltage = 300e3 # V

maxIterations = 12
fixProbeVal = 2
fixPosNum = 0
purePhaseVal = 0

lowpassCutoff = 0.210
highpassCutoff = None
globalAffineCorrect = None

comRotationForce = -90
comTransposeForce = False

defocusValInitialGuess = 2000e-9
stepsizePtycho = 0.1

datasetScanSize = (128,128)

binBy = 2
batchDiv = 32

######################################################################
### Rest is automatic ################################################
######################################################################

# Get gain maps
gainmap_brightField = np.asarray(Image.open('/em_data/berk/AllData/Calibrations/ELA/flatfield_2023_19_05.tif'),dtype=float)[:,514-256:514+256]
gainmap_darkField = np.asarray(Image.open('/em_data/berk/AllData/Calibrations/ELA/darkfield_2023_19_05.tif'),dtype=float)[:,514-256:514+256]
gainmap_brightField_cropped = np.copy(gainmap_brightField[0:256,0:256])
gainmap_darkField_cropped = np.copy(gainmap_darkField[0:256,0:256])

# Start processing
for fileIndex in fileIndexArray:
    
    # Load the data
    with nxopen(datasetFolder+"/pos_"+str(fileIndex)+".h5") as currentFile:
        allPatterns = np.array(currentFile.entry.data.data[:,:,:])
    unorderedPatterns = np.copy(allPatterns[:,:,514-256:514+256])
    unorderedPatterns_cropped = np.copy(unorderedPatterns[:,0:256,0:256])
    del(currentFile)
    del(allPatterns)
    
    # Apply flat-field correction
    correctedFrames, flatfieldGainMask, deadPixelMask = applyFlatfieldCorrection(
    unorderedPatterns_cropped,
    brightfield=gainmap_brightField_cropped,
    darkfield=gainmap_darkField_cropped)
    
    # Arange patterns
    currentDataset = arrangePattern(correctedFrames,datasetScanSize[0],datasetScanSize[1]).astype(np.uint8)
    
    # Bin dataset
    dataCurrent = currentDataset[4:-4,4:-4] # Once can crop the initial dataset here
    dataset = py4DSTEM.DataCube(data = dataCurrent).bin_Q(binBy)
    
    # Get vacuum probe
    vacuumProbe = fourier_resample(
        np.load(datasetFolder+"/vacuumProbe.npy"),
        output_size=(dataset.Qshape),
        force_nonnegative=True,
    )
    
    # Get the mean diffraction pattern and estimate calibrations
    dataset.get_dp_mean();
    probe_semiangle, probe_qx0, probe_qy0 = py4DSTEM.process.calibration.get_probe_size(
    dataset.tree('dp_mean').data**(1/1))
    
    # Set the pixel sizes, using the known values:
    dataset.calibration.set_R_pixel_size(R_pixel)
    dataset.calibration.set_R_pixel_units('A')
    dataset.calibration.set_Q_pixel_size(SemiConvAngle / probe_semiangle)
    dataset.calibration.set_Q_pixel_units('mrad')
    
    ##################################
    # Parallax, to estimate defocus
    with cp.cuda.Device(GPU_DeviceNum):
        parallax = py4DSTEM.process.phase.ParallaxReconstruction(
            energy = 300e3, 
            datacube = dataset,
            verbose = True,
            device='gpu'
        ).preprocess(
            defocus_guess= defocusValInitialGuess*1e10,
            rotation_guess= comRotationForce
        )
        parallax.reconstruct(
            max_iter_at_min_bin = 4,
            min_alignment_bin=32
        )
        parallax.aberration_fit(
            # plot_CTF_compare = True,
        )
        parallax.aberration_correct()
        defocusValueParallaxGuess = parallax.aberration_C1
        rotationGuess = parallax.rotation_Q_to_R_rads * 180 / np.pi
        
        cp.get_default_memory_pool().free_all_blocks()
        cp.get_default_pinned_memory_pool().free_all_blocks()

    cp.get_default_memory_pool().free_all_blocks()
    cp.get_default_pinned_memory_pool().free_all_blocks()
    
    
    ##################################
    # Ptychographic reconstruction
    
    object_type_val = 'potential'
    object_positivity_val = False # because of object_type=potential
    
    with cp.cuda.Device(GPU_DeviceNum):
        ptycho = py4DSTEM.process.phase.SingleslicePtychographicReconstruction(
            datacube=dataset,
            device = 'gpu',
            energy = AccVoltage,
            vacuum_probe_intensity = vacuumProbe,
            defocus = -defocusValueParallaxGuess,
            #polar_parameters = aberrationsList,
            #semiangle_cutoff=SemiConvAngle,
            object_padding_px=(16,16),
            verbose=True,
            object_type=object_type_val,
        ).preprocess(
            plot_center_of_mass = False, 
            plot_rotation = False, 
            plot_probe_overlaps = False, 
            force_com_rotation = rotationGuess,
            force_com_transpose = comTransposeForce,
            diffraction_intensities_shape = (192,192), # need to upsample to fit probe
        )

        ptycho = ptycho.reconstruct(
            reset = True,
            store_iterations = True,
            step_size = stepsizePtycho,
            max_iter = maxIterations,
            fix_probe_iter = fixProbeVal,
            pure_phase_object_iter = purePhaseVal,
            q_lowpass= lowpassCutoff,
            q_highpass = highpassCutoff,
            fix_positions_iter=fixPosNum,
            global_affine_transformation=globalAffineCorrect,
            max_batch_size = ptycho._num_diffraction_patterns//batchDiv,
            object_positivity=object_positivity_val,
        )
        
        cp.get_default_memory_pool().free_all_blocks()
        cp.get_default_pinned_memory_pool().free_all_blocks()

    cp.get_default_memory_pool().free_all_blocks()
    cp.get_default_pinned_memory_pool().free_all_blocks()
    
    ##################################
    # Export
    
    # Crop image
    result_potential = cropDownTo(ptycho.object_cropped,(1300,1300))
    
    # Export image
    pixelSizeObj = ptycho.sampling[0] * 1e-10 # [m]
    outputFileName = saveToFolder + '/' + str(fileIndex)+'_pot.mrc'
    exportMRC(result_potential,outputFileName,pixelSize=pixelSizeObj) # ,defocusValue=defocusObj,stepSize=stepsizeObj)
    np.save(saveToFolder + '/' + str(fileIndex)+'_pot.npy',result_potential)


