""" This little script is used to distiguish differences in the difference map
from just noise. Here this is done by splitting the data of each conformation 
into two. This is the traditional way of doiung it.
"""

__author__ = "Marcel Arheit (marcel.arheit@unibas.ch)"
__version__ = "$Revision: 1.3 $"
__date__ = "$Date: 31.05.2013 $"
__copyright__ = "Copyright (c) 2013 Marcel Arheit"
__license__ = "GNU GENERAL PUBLIC LICENSE (GPL) Version 2"


import sys
import os
from selections import *

if __name__ == "__main__":
	no_args = len(sys.argv)
	if no_args < 2:
            exit("usage: "+os.path.basename(sys.argv[0])+" 2dx_merge_dirfile.dat")
	else:
	    selection_file = sys.argv[1]
	    dir1 = get_parent_dir(selection_file)
            merge_dir = get_absolute_dir_path(selection_file)
            diffmap_dir = os.path.join(merge_dir, "diffmap")
            output_dir = create_output_dir(diffmap_dir, "selections")
            print(" output directory:"+output_dir)
            
            if no_args > 3:
                output_file_base = sys.argv[3]
            else:
                output_file_base = "diffmap_selection"
            [output_file1, output_file2] = get_selection_file_names(output_file_base)
            selection1 = parse_selection_file(selection_file)
            selection1 = append_dir2selection(selection1, dir1)
            [selection1_a,selection1_b] = split_selection_random(selection1)
            write_selection2file(selection1_a, output_file_base + "1a.dat")
            write_selection2file(selection1_b, output_file_base + "1b.dat")
            if no_args > 2:
                selection_file2 = sys.argv[2]
	        dir2 = get_parent_dir(selection_file2)
                print("directory 2:"+dir2)
                selection2 = parse_selection_file(selection_file2)
                selection2 = append_dir2selection(selection2, dir2)
                [selection2_a,selection2_b] = split_selection_random(selection2)
                write_selection2file(selection2_a, output_file_base + "2a.dat")
                write_selection2file(selection2_b, output_file_base + "2b.dat")
