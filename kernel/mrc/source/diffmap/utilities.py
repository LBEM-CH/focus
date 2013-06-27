"""A collection of utlility functions.

"""


__author__ = "Marcel Arheit (marcel.arheit@unibas.ch)"
__version__ = "$Revision: 1.0 $"
__date__ = "$Date: 24.06.2013 $"
__copyright__ = "Copyright (c) 2013 Marcel Arheit"
__license__ = "GNU GENERAL PUBLIC LICENSE (GPL) Version 2"

import os
from mrcimage import *

def list_files_in_dir(directory, ext=""):
    "reads files in directory with the specified extension"
    file_list = [  os.path.join(directory, f) for f in os.listdir(directory) if f.endswith(ext) ]
    return file_list


def get_mrc_image(filepath):
    "reads the mrc image specified by the filepath"
    with open(filepath,'r') as mrcFile:
        return MRCImage(mrcFile)

def save_mrc_image(image, mrc, output_file):
    "saves the image to the specified output file as an mrc file"
    mrc.setImage(image)
    mrc.tofile(output_file)
    
