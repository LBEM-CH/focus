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

        #print(':: amplitude scale: '+str(amp_scale))
	reflection_list2 = scale_amplitudes(reflection_list2, amp_scale)
        #print(":: reference amplitude 1: "+str(amp1))
        #print(":: reference amplitude 2: "+str(amp2))
	amp2_scaled = get_reference_amplitude(reflection_list2)
        #print(":: reference amplitude 2 scaled: "+str(amp2_scaled))
	
        #TODO: the amplitudes should be scaled to have the same mean
        #[reflection_list1, reflection_list2] = scale_all_amplitudes(reflection_list1, reflection_list2)

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
        
	if no_args > 7:
	    #[sig_refs1, sig_refs2] = determine_significant_reflections(sym_refs1,sym_refs2, float(sys.argv[7]))
            [sig_idx1, sig_idx2] = determine_significance_indices(sym_refs1, sym_refs2, float(sys.argv[7]))
	else:
	    #[sig_refs1, sig_refs2] = determine_significant_reflections(sym_refs1, sym_refs2)
            [sig_idx1, sig_idx2] = determine_significance_indices(sym_refs1, sym_refs2)
        print(str(sig_idx1))

        sig_refs1 = get_significant_reflections(merged_refs1, sig_idx1)
        sig_refs2 = get_significant_reflections(merged_refs2, sig_idx2)
        
        hkl_file1 = os.path.join(file_path1,'avrg2D_sig1.hkl')
	hkl_file2 = os.path.join(file_path1,'avrg2D_sig2.hkl')
	write_hkl_file(sig_refs1, hkl_file1)	
	write_hkl_file(sig_refs2, hkl_file2)	

