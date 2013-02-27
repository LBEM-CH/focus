import sys
import os 
from diffmap import *
from symmetry import *

if __name__ == '__main__':
	aph_file1=sys.argv[1]
	aph_file2=sys.argv[2]
	file_path1=os.path.dirname(sys.argv[1])
	file_path2=os.path.dirname(sys.argv[2])
	no_args = len(sys.argv)
	reflection_list1 = parse_aph_file(aph_file1)
	reflection_list2 = parse_aph_file(aph_file2)
	[reflection_list1, reflection_list2] = scale_all_amplitudes(reflection_list1, reflection_list2)
	if no_args == 3:
	    merged_refs1 = merge_reflections(reflection_list1)
	    merged_refs2 = merge_reflections(reflection_list2)
	elif no_args == 4:
	    merged_refs1 = merge_reflections(reflection_list1, float(sys.argv[3]))
	    merged_refs2 = merge_reflections(reflection_list2, float(sys.argv[3]))
	elif no_args == 5:
	    merged_refs1 = merge_reflections(reflection_list1, float(sys.argv[3]),int(sys.argv[4]))
	    merged_refs2 = merge_reflections(reflection_list2, float(sys.argv[3]),int(sys.argv[4]))
	elif no_args >= 6:
	    merged_refs1 = merge_reflections(reflection_list1, float(sys.argv[3]),int(sys.argv[4]),1.0/float(sys.argv[5]))
	    merged_refs2 = merge_reflections(reflection_list2, float(sys.argv[3]),int(sys.argv[4]),1.0/float(sys.argv[5]))

        #average amplitudes and phase like in 2dx_avrgamphs.for
        averaged_refs1 = average_reflections_by_idx(merged_refs1)
        averaged_refs2 = average_reflections_by_idx(merged_refs2)
        
        #TODO: get significant reflections by mixed merge methods
        hkl_file1 = os.path.join(file_path1,'avrg2D_sig1.hkl')
	hkl_file2 = os.path.join(file_path1,'avrg2D_sig2.hkl')
	write_hkl_file(averaged_refs1, hkl_file1)	
	write_hkl_file(averaged_refs2, hkl_file2)	

