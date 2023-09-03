#
# Ptychography sub-script. 
#
import os,sys
import math
import numpy as np
import py4DSTEM

print('Using py4DSTEM version ' + py4DSTEM.__version__)

from py4DSTEM.process.utils import fourier_resample

import cupy as cp
from matplotlib import pyplot as plt
import matplotlib.image
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


# User settings
fileIndexArray = np.arange(17,116+1) # np.arange(144,240+1) #np.arange(17,116+1)
GPU_DeviceNum = 0
# datasetFolder = "/em_data/berk/AllData/data4DSTEM/2023_07_11/square1"
datasetFolder = "."
# saveToFolder = "/em_data/berk/AllData/data4DSTEM/2023_07_11/allImages_potential_variedRot/"
saveToFolder = "."
lowpassCutoff = 0.210
highpassCutoff = None
globalAffineCorrect = None
comRotationForce = -90
comTransposeForce = False
defocusValInitialGuess = 2000e-9
stepsizePtycho = 0.1
datasetScanSize = (128,128)


print ('Provided arguments = ' + str(len(sys.argv)))
print ('These are:')
for i in range(0,len(sys.argv)):
    print ('sys.argv[' + str(i) + '] = ' + str(sys.argv[i]))

if len(sys.argv) != 18:
     sys.exit("Wrong usage.")


datasetAddress = sys.argv[1]
R_pixel        = float(sys.argv[2])
SemiConvAngle  = float(sys.argv[3])
AccVoltage     = float(sys.argv[4]) * 1000.0
defocusVal     = float(sys.argv[5])
CsVal          = float(sys.argv[6])
fixProbeVal    = int(float(sys.argv[7]))
purePhaseVal   = int(float(sys.argv[8]))
fixPosNum      = int(float(sys.argv[9]))
binBy          = int(float(sys.argv[10]))
maxIterations  = int(float(sys.argv[11]))
batchDiv       = int(float(sys.argv[12]))
gainmap_brightField_file = sys.argv[13]
gainmap_darkField_file   = sys.argv[14]
scriptname     = sys.argv[15]
GPU_DeviceNum  = int(float(sys.argv[16]))
Dataset_crop   = int(float(sys.argv[17]))
# 
#
print (' ')
print ('Parameters received are:')
#
print ('datasetAddress = ' + str(datasetAddress))
print ('R_pixel        = ' + str(R_pixel       ))
print ('SemiConvAngle  = ' + str(SemiConvAngle ))
print ('AccVoltage     = ' + str(AccVoltage    ))
print ('defocusVal     = ' + str(defocusVal    ))
print ('CsVal          = ' + str(CsVal         ))
print ('fixProbeVal    = ' + str(fixProbeVal   ))
print ('purePhaseVal   = ' + str(purePhaseVal  ))
print ('fixPosNum      = ' + str(fixPosNum     ))
print ('binBy          = ' + str(binBy         ))
print ('maxIterations  = ' + str(maxIterations ))
print ('batchDiv       = ' + str(batchDiv      ))
print ('gainmap_brightField_file = ' + str(gainmap_brightField_file))
print ('gainmap_darkField_file  = ' + str(gainmap_darkField_file))
print ('scriptname     = ' + str(scriptname))
print ('GPU_DeviceNum  = ' + str(GPU_DeviceNum))
print ('Dataset_crop   = ' + str(Dataset_crop))

#############################################################################
gainmap_brightField = np.asarray(Image.open(gainmap_brightField_file),dtype=float)[:,514-256:514+256]
gainmap_darkField = np.asarray(Image.open(gainmap_darkField_file),dtype=float)[:,514-256:514+256]
gainmap_brightField_cropped = np.copy(gainmap_brightField[0:256,0:256])
gainmap_darkField_cropped = np.copy(gainmap_darkField[0:256,0:256])
#############################################################################
#
with nxopen(datasetAddress) as currentFile:
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
dmin = Dataset_crop
dmax = -1 * Dataset_crop
dataCurrent = currentDataset[dmin:dmax,dmin:dmax] # Once can crop the initial dataset here
print('binBy = ' + str(binBy))
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
result_potential = cropDownTo(ptycho.object_cropped,(256,256))
matplotlib.image.imsave('outfile.png',result_potential)

# Export image
#########################################
pixelSizeObj = ptycho.sampling[0] * 1e-10 # [m]
outputFileName = saveToFolder + '/' + 'output_pot.mrc'
exportMRC(result_potential,outputFileName,pixelSize=pixelSizeObj) # ,defocusValue=defocusObj,stepSize=stepsizeObj)
np.save(saveToFolder + '/' + 'output_pot.npy',result_potential)

ptycho_rotation = rotationGuess
ptycho_defocus = -defocusValueParallaxGuess

file = open('LOGS/' + scriptname + '.results', 'a')
file.write('set ptycho_rotation = "' + str(ptycho_rotation) + '"\n')
file.write('set ptycho_defocus = "' + str(ptycho_defocus) + '"\n')
file.close()
#

