"""Framework for getting symmetry related lattice reflections.

This program was originally written for the significant difference map 
method implemented in 2dx_diffmap_sub.py. Where symmetrization was needed,
but in contrast to the symmetrization in 2dx_hkl_sym4.for, the reflections
should not be merged to one reflection, but gathered as a list of reflectcions.

"""

__author__ = "Marcel Arheit (marcel.arheit@unibas.ch)"
__version__ = "$Revision: 1.3 $"
__date__ = "$Date: 04.02.2013 $"
__copyright__ = "Copyright (c) 2013 Marcel Arheit"
__license__ = "GNU GENERAL PUBLIC LICENSE (GPL) Version 2"

import os
import sys
from math import copysign
from collections import defaultdict
import diffmap 

def stripnulls(data):
    "strip whitespace and nulls"
    return data.replace("\00", " ").strip()

class Symmetry(dict):
    
    """
    determines the symmetrie related reflections according to the given plane group
    
	The following table comes from the ALLSPACE program:

		Table of phase comparisons to be made
			  -  not comparable       
			  1  directly identical
			  H  differ by 180 * H            JSIMPL  = number to compare directly
			  K  differ by 180 * K            JSCREW  = number to compare + 180 * M
			  HK differ by 180 * (H+K)        where M = H*JH180 + K*JK180

		  SPACEGROUP  H=-h +h -h +k +k -k -k +h -h +k -k -h +h -h +h  JSIMPL
				   H=                                 -k +k -k +k     JSCREW
		ref in
		 prog # symb  K=+k -k -k +h -h +h -h -h +h -h +h +h -h +k -k         JH180
				   K=                     -k +k -k +k                         JK180

		 1    1   p1     -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   0  0   -   -
		 2    2   p2     -  -  1  -  -  -  -  -  -  -  -  -  -  -  -   1  0   -   -
		 3    3b  p12    1  -  -  -  -  -  -  -  -  -  -  -  -  -  -   1  0   -   -
		 5    4b  p121   K  -  -  -  -  -  -  -  -  -  -  -  -  -  -   0  1   -  180
		 7    5b  c12    1  -  -  -  -  -  -  -  -  -  -  -  -  -  -   1  0   -   -
		 9    6   p222   1  1  1  -  -  -  -  -  -  -  -  -  -  -  -   3  0   -   -
		10    7b  p2221  H  H  1  -  -  -  -  -  -  -  -  -  -  -  -   1  2  180  -
		12    8   p22121 HK HK 1  -  -  -  -  -  -  -  -  -  -  -  -   1  2  180 180
		13    9   c222   1  1  1  -  -  -  -  -  -  -  -  -  -  -  -   3  0   -   -
		14    10  p4     -  -  1  -  1  1  -  -  -  -  -  -  -  -  -   3  0   -   -
		15    11  p422   1  1  1  1  1  1  1  -  -  -  -  -  -  -  -   7  0   -   -
		16    12  p4212  HK HK 1  1  HK HK 1  -  -  -  -  -  -  -  -   3  4  180 180
		17    13  p3     -  -  -  -  -  -  -  -  -  1  -  1  -  -  -   2  0   -   -
		18    14  p312   -  -  -  -  -  -  1  -  1  1  -  1  -  -  1   5  0   -   -
		19    15  p321   -  -  -  1  -  -  -  1  -  1  -  1  -  1  -   5  0   -   -
		20    16  p6     -  -  1  -  -  -  -  -  -  1  1  1  1  -  -   5  0   -   -
		21    17  p622   -  -  1  1  -  -  1  1  1  1  1  1  1  1  1   11 0   -   -
		
		this table is manifested in the class attribute symmetrieTable

    """
    planegroup_number = {}
    planegroup_number['p1']     = 1
    planegroup_number['p2']     = 2
    planegroup_number['p12_b']  = 3
    planegroup_number['p121_b'] = 4
    planegroup_number['c12_b']  = 5
    planegroup_number['p222']   = 6
    planegroup_number['p2221b'] = 7
    planegroup_number['p22121'] = 8
    planegroup_number['c222']   = 9
    planegroup_number['p4']     = 10 
    planegroup_number['p422']   = 11 
    planegroup_number['p4212']  = 12 
    planegroup_number['p3']     = 13 
    planegroup_number['p312']   = 14 
    planegroup_number['p321']   = 15 
    planegroup_number['p6']     = 16 
    planegroup_number['p622']   = 17 

    symmetry_table = [[0]*17]*17	
    symmetry_table[0]  = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
    symmetry_table[1]  = [0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
    symmetry_table[2]  = [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0] 
    symmetry_table[3]  = [3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
    symmetry_table[4]  = [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
    symmetry_table[5]  = [1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
    symmetry_table[6]  = [2,2,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0]
    symmetry_table[7]  = [4,4,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1]
    symmetry_table[8]  = [1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
    symmetry_table[9]  = [0,0,1,0,1,1,0,0,0,0,0,0,0,0,0,0,0]
    symmetry_table[10] = [1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0]
    symmetry_table[11] = [4,4,1,1,4,4,1,0,0,0,0,0,0,0,0,1,1]
    symmetry_table[12] = [0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0]
    symmetry_table[13] = [0,0,0,0,0,0,1,0,1,1,0,1,0,0,1,0,0]
    symmetry_table[14] = [0,0,0,1,0,0,0,1,0,1,0,1,0,1,0,0,0]
    symmetry_table[15] = [0,0,1,0,0,0,0,0,0,1,1,1,1,0,0,0,0]
    symmetry_table[16] = [0,0,1,1,0,0,1,1,1,1,1,1,1,1,1,0,0]

    """ 
    returns the modified miller index according to the symmetry operation.

    Below is a matrix hodling how the miller index of the reflection has to  
    be modfied for the symmetry opperation accosiated with the plane group.
    This is taken from the program ALLSPACE:
        
        H=-h +h -h +k +k -k -k +h -h +k -k -h +h -h +h  JSIMPL
        H=                                 -k +k -k +k     JSCREW
        K=+k -k -k +h -h +h -h -h +h -h +h +h -h +k -k         JH180
        K=                     -k +k -k +k                         JK180
      
    
    INTEGER SYMINDEX(15,2)
      DATA SYMINDEX / 
     . -1, 1,-1, 2, 2,-2,-2, 1,-1, 2,-2,-3, 3,-3 ,3,
     .  2,-2,-2, 1,-1, 1,-1,-3, 3,-3, 3, 1,-1, 2,-2 /
      
    """
    symmetry_index = [(-1,2),(1,-2),(-1,-2),(2,1),(2,-1),(-2,1),(-2,-1),(1,-3),(-1,3),(2,-3),(-2,3),(-3,1),(3,-1),(-3,2),(3,-2)]


    def __init__(self, planegroup='p1'):
        self["planegroup"] = planegroup
        #init the symetrized reflection list
        self.symmetrized_reflections = defaultdict(list) 
        #TODO: have the reflections as an argumenet for init
        #self.reflections = reflections
        if planegroup in  self.planegroup_number:
            planegroup_no = self.planegroup_number[planegroup]
            self["planegroup_number"] = planegroup_no
        else:
            exit("Error: the planegroup "+planegroup+" is not supported.")

    def symmetrize(self, reflections):
        #shifted_refs = self.correct_phases(reflections)
        self.__symmetrize_list(reflections)
        return self.symmetrized_reflections

    def correct_phases(self, reflections):
        "corrects the phases of the reflections"
        for key in reflections:
            self.__shift_phases(reflections[key])
        return reflections

    def get_symmetrized_list(self):
        return self.symmetrized_reflections

    def __symmetrize_list(self, reflections):
        "groups the reflections with its symmetry mates"
        self.__add_reflections(reflections)
        self.__symmetrize_reflection_list(reflections)
        return self.symmetrized_reflections
        
        
        
    def __add_reflections(self, reflections):
        "adds the original reflection list and the Friedel mates to the symmetrized list"
        for key in sorted(reflections.keys()):
            h = key[0]
            k = key[1]
            if len(key) == 3:
                l = key[2]
            else:
                l = 0
            #add the reflection
            self.__add_reflection(h,k,l,reflections[key])
    
    def __shift_phases(self, reflections):
        "makes sure taht the reflections phases are between [-180,180]"
        corrected_refs = []
        for ref in reflections:
            phase = ref.phase
            while phase > 180.0:
                phase = phase - 360.0
            while phase < -180.0:
                phase = phase + 360.0
            ref.phase = phase
            corrected_refs.append(ref)
        return corrected_refs

   
    def __add_reflection(self,h,k,l,idx_reflections):
        "adds the original reflection and its Friedel mate to the symmetrized list"
        # appending would be a problem if the dict was empty, but since we are
        # using a defaulldict it is not an issue
        self.symmetrized_reflections[h,k,l].extend(idx_reflections)
        self.symmetrized_reflections[-h,-k,-l].extend(self.friedel_mates(idx_reflections))

    def __symmetrize_reflection_list(self, reflections):
        "adding the symmetrized mates of the refections to the symmetrized list"
        planegroup = self["planegroup_number"]
        # getting the symmetry operations for this plane group 
        # (planegroup starts from 1 but indices always start from 0)
        symmetry_operations = self.symmetry_table[planegroup-1]
        # check all symmetry ops, except for the last two
        # TODO: should the last two sym ops be considered?
        for i in range(len(symmetry_operations)-2):
            if symmetry_operations[i] > 0:
                print(":: symmetry operator: "+str(i))
                self.__symmetrize_reflections(i, reflections)

    def __symmetrize_reflections(self, sym_op, reflections):
        "symmetrize all reflections according the symmetry operation"
        for index in sorted(reflections.keys()):
            sym_index = self.modify_miller_index(index,sym_op)
            self.__add_sym_reflection(sym_index,sym_op,reflections[index])
            #print(str(key)+" -> "+str(sym_index))

    def modify_miller_index(self, index, sym_op):
        "modifies the given miller index according to the symmetry operation"
        idx_modifier = self.symmetry_index[sym_op]
        h = self.__modify__index(index,idx_modifier[0])
        k = self.__modify__index(index,idx_modifier[1])
        if len(index) > 2:
            l = index[2]
        else:
            l = 0
        return (h,k,l)

    def __add_sym_reflection(self, miller_index,sym_operation,idx_reflections):
        ""
        h = miller_index[0]
        k = miller_index[1]
        l = miller_index[2]
        if sym_operation == 2:
            phase_change = h * 180.0
        elif sym_operation == 3:
            phase_change = k * 180.0
        elif sym_operation == 4:
            phase_change = (h + k) * 180.0
        else:
            phase_change = 0.0
        sym_idx_reflections = []
        for ref in idx_reflections:
            #TODO: ucomment this for symmetrization
            #ref.phase = (ref.phase + phase_change) % 360.0
            sym_idx_reflections.append(ref)
        self.__add_reflection(h,k,l,sym_idx_reflections)
        return sym_idx_reflections 
            

        
    def __modify__index(self, index ,modifier):
        "translates the index modifier to the actual modified index"
        abs_modifier = abs(modifier)
        if abs_modifier == 1:
            idx = index[0] * copysign(1,modifier)
        elif abs_modifier == 2:
            idx = index[1] * copysign(1,modifier)
        else:
            idx = (index[0]+index[1]) * copysign(1,modifier)
        return idx

    def friedel_mates(self, reflections):
        "returns the Friedel mates of the reflections"
        friedel_mates = [] 
        for ref in reflections:
            ref.phase = -1.0*ref.phase 
            friedel_mates.append(ref)
        return friedel_mates

    def print_symmetrized_list_keys(self):
        for key in sorted(self.symmetrized_reflections.keys()):
            print(str(key))

    def __str__(self):
        string = "planegroup "+self["planegroup"]+"\n"
        for key in sorted(self.symmetrized_reflections.keys()):
            string = string+"miller index "+str(key)+"\n"
            idx_reflections = self.symmetrized_reflections[key]
            for ref in idx_reflections:
                string = string+"\t"+str(ref)+"\n"
        return string
        
        


            
if __name__ == "__main__":
	no_args = len(sys.argv)
	if no_args < 3:
		print("usage: "+os.path.basename(sys.argv[0])+" merge.aph planegroup")
	else:
		filename = sys.argv[1]
		planegroup = sys.argv[2]
	        file_path=os.path.dirname(filename)
		reflection_list = diffmap.parse_aph_file(filename)
		reflections = diffmap.merge_reflections(reflection_list)
		symmetry = Symmetry(planegroup)
		sym_refs = symmetry.symmetrize(reflections)
                merged_refs = diffmap.average_reflections_by_idx(sym_refs)
                outfile = os.path.join(file_path, "sym_sig.hkl")
                diffmap.write_sym_hkl_file(merged_refs, outfile)
                #print(symmetry)
                #sym.print_symmetrized_list_keys()

