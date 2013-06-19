""" This little script is used to distiguish differences in the difference map
from just noise. This is achieved by either merging the two classes together and then
splitng them into two or just splitting the classes separately.

"""

__author__ = "Marcel Arheit (marcel.arheit@unibas.ch)"
__version__ = "$Revision: 1.3 $"
__date__ = "$Date: 24.02.2013 $"
__copyright__ = "Copyright (c) 2013 Marcel Arheit"
__license__ = "GNU GENERAL PUBLIC LICENSE (GPL) Version 2"


import sys
import os
import random

output_dir = "diffmap/selections"

def get_absolute_dir_path(file_path):
    """returns the absolute directory path
    where the specified file lies"""
    dirname = os.path.dirname(file_path)
    absdir = os.path.abspath(dirname)
    return absdir

def get_parent_dir(file_path):
    """gets the parent directory of the directory where the selectio file
    actually lies"""
    absdir = get_absolute_dir_path(file_path)
    parent_dir = os.path.normpath(os.path.join(absdir, os.pardir))
    return parent_dir

def create_output_dir(abs_path, output_dir):
    """creates the output directory if it does not exist"""
    output_path = os.path.join(abs_path, output_dir)
    if not os.path.isdir(output_path):
        os.mkdir(output_path)
    return output_path


def get_selection_file_names(base, iteration=1 , conformation=0, ext=".dat"):
    """returns the name of the file names for the selections based on the base"""
    file_base = base + str(iteration) +"_" + str(conformation)
    selection1_filename = file_base + "a" +ext
    selection2_filename = file_base + "b" +ext
    return [selection1_filename, selection2_filename]
    

def parse_selection_file(selection_filepath):
    """parses the selection file"""
    with open(selection_filepath,'r') as selection_file:
        return [line for line in selection_file]

def split_selection(selection):
    """splits the selection file into two selections of even and uneven entries"""
    selection_a = [] 
    selection_b = [] 
    line_no = 0 
    for line in selection:
        if line_no % 2 == 0:
            selection_a.append(line)
        else:
            selection_b.append(line)
        line_no = line_no + 1
    return [selection_a, selection_b]

def split_selection_random(selection):
    """splits the selection file randomly into two selections"""
    n = len(selection)
    random.shuffle(selection)
    selection_a = selection[0:n/2] 
    selection_b = selection[n/2:n] 
    return [selection_a, selection_b]



def append_dir2selection(selection, directory):
    """appends the directory where the file lies to each entry of the selection"""
    return [os.path.join(directory, entry) for entry in selection]

def append_dir2selections(selection1, selection2, directory):
    """appends the directory to selections """
    abs_selection1 = append_dir2selection(selection1)
    abs_selection2 = append_dir2selection(selection2)
    return [abs_selection1, abs_selection2]

def read_selection_file(selection_file):
    """reads the contens of the file adds it to a list and prepends the aproriate directory"""
    selection = parse_selection_file(selection_file) 
    parent_dir = get_parent_dir(selection_file)
    selection = append_dir2selection(selection, parent_dir)
    return selection


def write_selection2file(selection, output_file):
    "writes the selection to the file"
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)
    output_filepath = os.path.join(output_dir, output_file)
    with open(output_filepath,'w') as output:
        for item in selection:
            #abspath = os.path.normpath(os.path.join(directory,item)) 
            output.write(item)
