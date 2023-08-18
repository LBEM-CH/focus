#
# Ptychography sub-script. 
#
import os,sys
import matplotlib.pyplot as plt
import math
import numpy as np
import py4DSTEM

from matplotlib import pyplot as plt

print('Using py4DSTEM version ' + py4DSTEM.__version__)


print ('Provided arguments = ' + str(len(sys.argv)))
print ('These are:')
for i in range(0,len(sys.argv)):
    print ('sys.argv[' + str(i) + '] = ' + str(sys.argv[i]))

if len(sys.argv) != 13:
     sys.exit("Wrong usage.")


datasetAddress = sys.argv[1]
R_pixel        = sys.argv[2]
SemiConvAngle  = sys.argv[3]
AccVoltage     = sys.argv[4]
defocusVal     = sys.argv[5]
CsVal          = sys.argv[6]
fixProbeVal    = sys.argv[7]
purePhaseVal   = sys.argv[8]
fixPosNum      = sys.argv[9]
binBy          = sys.argv[10]
maxIterations  = sys.argv[11]
batchDiv       = sys.argv[12]
# 
#############################################################################
# Rest is automatic #########################################################
#############################################################################

# Load the data
dataset = py4DSTEM.io.datastructure.DataCube(
    data=np.load(datasetAddress)
    #data=np.load(datasetAddress)[127:256,20:256]
).bin_Q(binBy)

# Get the dose
avgDose = np.sum(dataset.data) / (R_pixel * np.shape(dataset.data)[0] * R_pixel * np.shape(dataset.data)[1])
print ('Average dose = ' + str(avgDose) + ' e/A2')

# Let's examine the mean diffraction space image - first we compute it:
dataset.get_dp_mean();

# Plot the mean diffraction pattern
py4DSTEM.show(
    dataset.tree['dp_mean'],
)

# Estimate the radius of the BF disk, and the center coordinates
probe_semiangle, probe_qx0, probe_qy0 = py4DSTEM.process.calibration.get_probe_size(dataset.tree['dp_mean'].data)

# Set the pixel sizes, using the known values:
dataset.calibration.set_R_pixel_size(R_pixel)
dataset.calibration.set_R_pixel_units('A')
dataset.calibration.set_Q_pixel_size(SemiConvAngle / probe_semiangle)
dataset.calibration.set_Q_pixel_units('mrad')

# Ptychography Preprocess!
ptycho = py4DSTEM.process.phase.PtychographicReconstruction(
    dataset,
    verbose=True,
    energy=AccVoltage,
    semiangle_cutoff=SemiConvAngle,
    rolloff=0.0,
    defocus=defocusVal,
    Cs=CsVal,
    device='gpu',
    object_padding_px=(16,16),
).preprocess(
    force_com_rotation=90,
    force_com_transpose=True,
    plot_center_of_mass = False,
    plot_rotation=False,
    plot_probe_overlaps = False,
    fit_function='constant',
)

# Ptychography Reconstruct!
ptycho = ptycho.reconstruct(
    reset=True,
    progress_bar = True,
    store_iterations = True,
    max_iter = maxIterations,
    fix_probe_iter = fixProbeVal,
    pure_phase_object_iter = purePhaseVal,
    step_size = 0.125,
    normalization_min = 1.0,
    q_lowpass=0.065,
    #q_lowpass=0.050,
    fix_positions_iter=fixPosNum,
    positions_step_size=0.25,
    global_affine_transformation=True,
    max_batch_size=ptycho._num_diffraction_patterns//batchDiv,
).visualize(
    iterations_grid='auto'
)

# Visualize !

py4DSTEM.show(
    [
        np.abs(ptycho.object),
        np.angle(ptycho.object)
    ],
    figsize=(15,15),
    title = ['Amplitude','Phase'],
    ticks=False,
    cmap='inferno',
    vmin=0.200,
    vmax=0.999,
)

py4DSTEM.show(
    [
        np.abs(ptycho.object),
        np.angle(ptycho.object)
    ],
    figsize=(15,15),
    title = ['Amplitude','Phase'],
    ticks=False,
    cmap='gray',
)

py4DSTEM.show(
    [
        np.abs(ptycho.object),
        np.abs(np.fft.fftshift(np.fft.fft2(np.abs(ptycho.object))))**(1/2)
    ],
    figsize=(15,15),
    title = ['Amplitude','Spectra'],
    ticks=False,
    cmap='gray',
)

py4DSTEM.show(
    [
        np.angle(ptycho.object),
        np.abs(np.fft.fftshift(np.fft.fft2(np.angle(ptycho.object))))**(1/2)
    ],
    figsize=(15,15),
    title = ['Phase','Spectra'],
    ticks=False,
    cmap='gray',
)

py4DSTEM.show(
    np.angle(ptycho.object),
)

# np.save("/em_data/berk/AllData/data4DSTEM/2023_04_04/ptychoResult.npy",
# ptycho.object, allow_pickle=True, fix_imports=True)

print('The ptychographic reconstructed pixel-size is:')
print(str(ptycho.sampling) + ' A')
