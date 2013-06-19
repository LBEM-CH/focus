""" This little script is used to distiguish differences in the difference map
from just noise. This is achieved by merging the two classes together and then
splitng them.

"""

__author__ = "Marcel Arheit (marcel.arheit@unibas.ch)"
__version__ = "$Revision: 1.3 $"
__date__ = "$Date: 24.02.2013 $"
__copyright__ = "Copyright (c) 2013 Marcel Arheit"
__license__ = "GNU GENERAL PUBLIC LICENSE (GPL) Version 2"


import sys
import os
from optparse import OptionParser
from selections import *

if __name__ == "__main__":
    no_args = len(sys.argv)
    usage = 'usage: %prog [options] selection-file'
    parser = OptionParser(usage=usage)
    parser.add_option("-m", "--mode", 
            default="mix", 
            help="decides if the slections should be mix and merged or not.")
    parser.add_option("-n", "--iterations", 
            type=int, default=1, 
            help="the number of times the selection is randomly split.")
    parser.add_option("-o", "--output_file_base", 
            default="diffmap_split_selection", 
            help="is the filename output base for the selection files")
    (options, args) = parser.parse_args()	
    no_args = len(args)
    if no_args == 0:
        parser.error("you have to specify at least one selection file as argument.")
    if options.mode == "mix" and no_args < 2:
        parser.error("When usig the mixed merge mode you have to specify two selections, one for each conformation.")
    selection1 = read_selection_file(args[0])
    if no_args == 2:
        selection2 = read_selection_file(args[1])
    for i in range(options.iterations):
        [selection_a,selection_b] = split_selection_random(selection1)
        [output_file_a, output_file_b] = get_selection_file_names(options.output_file_base, i, 1)
        if no_args >= 2: 
            [selection2_a,selection2_b] = split_selection_random(selection2)
            if options.mode == "mix":
                [output_file_a, output_file_b] = get_selection_file_names(options.output_file_base, i)
                selection_a = selection_a + selection2_b
                selection_b = selection_b + selection2_a
            else:
                [output_file_a, output_file_b] = get_selection_file_names(options.output_file_base, i, 1)
                [output_file2_a, output_file2_b] = get_selection_file_names(options.output_file_base, i, 2)
                write_selection2file(selection2_a, output_file2_a)
                write_selection2file(selection2_b, output_file2_b)
        write_selection2file(selection_a, output_file_a)
        write_selection2file(selection_b, output_file_b)

