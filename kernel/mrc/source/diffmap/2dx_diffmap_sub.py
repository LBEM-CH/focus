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
	amp1 = get_reference_amplitude(reflection_list1)
	amp2 = get_reference_amplitude(reflection_list2)
	amp_scale = amp1/amp2
	print('amplitude scale: '+str(amp_scale))
	print(reflection_list2[0])
	reflection_list2 = scale_amplitudes(reflection_list2, amp_scale)
	print(reflection_list2[0])
	print(str(amp1)+" is the amplitude mean.")
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
	if no_args > 6:
            planegroup = sys.argv[6]
        else:
            planegroup = 'p1'
        
        sym1 = Symmetry(planegroup)
        sym_refs1 = sym1.symmetrize(merged_refs1)
        sym2 = Symmetry(planegroup)
        sym_refs2 = sym2.symmetrize(merged_refs2)
        
#        sym_refs1 = merged_refs1
#        sym_refs2 = merged_refs2
	if no_args > 7:
	    #[sig_refs1, sig_refs2] = determine_significant_reflections(sym_refs1,sym_refs2, float(sys.argv[7]))
            [sig_idx1, sig_idx2] = determine_significance_indices(sym_refs1, sym_refs2, float(sys.argv[7]))
	else:
	    #[sig_refs1, sig_refs2] = determine_significant_reflections(sym_refs1, sym_refs2)
            [sig_idx1, sig_idx2] = determine_significance_indices(sym_refs1, sym_refs2)
        print(str(sig_idx1))

        print(":: significant indices 1: "+str(len(sig_idx1)))
        print(":: significant indices 2: "+str(len(sig_idx2)))
        sig_refs1 = get_significant_reflections(merged_refs1, sig_idx1)
        sig_refs2 = get_significant_reflections(merged_refs2, sig_idx2)
        print(":: significant reflections 1: "+str(len(sig_refs1)))
        print(":: significant reflections 2: "+str(len(sig_refs2)))
        
        hkl_file1 = os.path.join(file_path1,'avrg2D_sig1.hkl')
	hkl_file2 = os.path.join(file_path1,'avrg2D_sig2.hkl')
	write_hkl_file(sig_refs1, hkl_file1)	
	write_hkl_file(sig_refs2, hkl_file2)	
	#write_hkl_file(average_reflections_by_idx(merged_refs1), hkl_file1)	
	#write_hkl_file(average_reflections_by_idx(merged_refs2), hkl_file2)	
	#sym_hkl_file1 = os.path.join(file_path1,'sym2D_sig1.hkl')
	#sym_hkl_file2 = os.path.join(file_path1,'sym2D_sig2.hkl')
	#write_hkl_file(sig_refs1, sym_hkl_file1)	
	#write_hkl_file(sig_refs2, sym_hkl_file2)	
	#print('reflections: '+str(len(reflection_list)))
	#print('**************************************************')
	#print('Merged Reflections Conformation 1 ****************')
	#print('**************************************************')
	#for item in merged_refs1:
	#	print(str(item)+'\n')
	#print('**************************************************')
	#print('Merged Reflections Conformation 2 ****************')
	#print('**************************************************')
	#for item in merged_refs2:
	#	print(str(item)+'\n')
	#print([str(item) for item in merged_refs])

