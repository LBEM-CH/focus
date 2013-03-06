import sys
import os 
from diffmap import *
from symmetry import *

if __name__ == '__main__':
	no_args = len(sys.argv)
	if no_args < 2:
		sys.exit('Usage: python '+sys.argv[0]+' merge.aph')
	aph_file=sys.argv[1]
	file_path=os.path.dirname(sys.argv[1])
	reflection_list = parse_aph_file(aph_file)
        reflection_list = scale_amplitudes2common_mean(reflection_list)
	#reflection_list = scale_all_amplitudes(reflection_list)
        #TODO: get significant reflections by mixed merge methods
        #aph_file = os.path.join(file_path,'merge_scaled.aph')
	write_aph_file(reflection_list, aph_file)	

