""" This little script is used to distiguish differences in the difference map
from just noise. This is achieved by merging the two classe together and then
splitng them into two.

"""

__author__ = "Marcel Arheit (marcel.arheit@unibas.ch)"
__version__ = "$Revision: 1.3 $"
__date__ = "$Date: 24.02.2013 $"
__copyright__ = "Copyright (c) 2013 Marcel Arheit"
__license__ = "GNU GENERAL PUBLIC LICENSE (GPL) Version 2"


import sys
import os



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
    

def parse_selection_file(selection_filepath):
    """parses the selection file"""
    with open(selection_filepath,'r') as selection_file:
        return [line for line in selection_file]

def split_selection(selection):
    """splits the selection file int to selections even and uneven entries"""
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


def append_dir2selection(selection, directory):
    """appends the directory where the file lies to each entry of the selection"""
    return [os.path.join(directory, entry) for entry in selection]

def append_dir2selections(selection1, selection2, directory):
    """appends the directory to selections """
    abs_selection1 = append_dir2selection(selection1)
    abs_selection2 = append_dir2selection(selection2)
    return [abs_selection1, abs_selection2]
    


def write_selection2file(selection, output_filepath):
    "writes the selection to the file"
    with open(output_filepath,'w') as output:
        for item in selection:
            #abspath = os.path.normpath(os.path.join(directory,item)) 
            output.write(item)

if __name__ == "__main__":
	no_args = len(sys.argv)
	if no_args < 2:
            exit("usage: "+os.path.basename(sys.argv[0])+" 2dx_merge_dirfile.dat")
	else:
	    selection_file = sys.argv[1]
	    dir1 = get_parent_dir(selection_file)
            print("directory 1:"+dir1)
            selection1 = parse_selection_file(selection_file)
            selection1 = append_dir2selection(selection1, dir1)
            [selection1_a,selection1_b] = split_selection(selection1)
            if no_args > 2:
                selection_file2 = sys.argv[2]
	        dir2 = get_parent_dir(selection_file2)
                print("directory 2:"+dir2)
                selection2 = parse_selection_file(selection_file2)
                selection2 = append_dir2selection(selection2, dir2)
                [selection2_a,selection2_b] = split_selection(selection2)
                # this makes sure that if both original lists are uneven the final
                # count is equal
                selection1 = selection1_a + selection2_b
                selection2 = selection1_b + selection2_a
                write_selection2file(selection1, 'diffmap_mixed_selection1.dat')
                write_selection2file(selection2, 'diffmap_mixed_selection2.dat')
            else:
                write_selection2file(selection1_a, 'diffmap_mixed_selection1.dat')
                write_selection2file(selection1_b, 'diffmap_mixed_selection2.dat')
