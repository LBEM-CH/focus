import sys
import os
from optparse import OptionParser
from plotting import *
from utilities import *

def list_selection_maps(selection_dir):
    """lists all the mrc files in the selection directory"""
    selection_maps = [ os.path.join(selection_dir, f) for f in os.listdir(selection_dir) if f.endswith('.mrc') ]
    return selection_maps

def pair_match(pair):
    "checks if pair elements only differ in the last 2 characters without the ending"
    #remove extension

    a = os.path.splitext(pair[0])[0]
    b = os.path.splitext(pair[1])[0]
    if a[0:-2] == b[0:-2]:
        return True
    else:
        return False

def remove_mismatched_pairs(pair_list):
    """checks if the filename of the pairs just differ in the ending, otherwise it removes that pair"""
    matched_pairs = [ pair for pair in pair_list if pair_match(pair)]
    return matched_pairs

     

def get_map_pairs(maplist):
    """returns a list of map pairs based on the file name"""
    pairs = zip(maplist[0::2], maplist[1::2])
    remove_mismatched_pairs(pairs)
    return pairs

def get_variation_from_pair(pair):
    file1 = pair[0]
    file2 = pair[1]
    im1 = getImage(file1)
    im2 = getImage(file2)
    varmap = getAbsDiffmap(im1,im2)
    return varmap

def get_mrc_image(filepath):
    "reads the mrc image specified by the filepath"
    with open(filepath,'r') as mrcFile:
        return MRCImage(mrcFile)

def get_variationmaps(map_pairs):
    "returns a list of variation maps for every pair"
    varmap_list = [get_variation_from_pair(pair) for pair in map_pairs ]
    return varmap_list

def get_maxmap(varmap_list):
    "returns max of all variations maps"
    varmap_array = np.array(varmap_list)
    maxmap = np.max(varmap_array,0)
    return maxmap 
 
def get_meanmap(varmap_list):
    "returns mean of all variations maps"
    varmap_array = np.array(varmap_list)
    meanmap = np.mean(varmap_array,0)
    return meanmap 

def get_meanmap(varmap_list):
    "returns mean of all variations maps"
    varmap_array = np.array(varmap_list)
    meanmap = np.mean(varmap_array,0)
    return meanmap

def get_rmsdmap(varmap_list):
    "returns the root mean square difference of all variation maps"
    square = [ var*var for var in varmap_list ]
    meanmap = get_meanmap(square)
    return np.sqrt(meanmap)



def get_varmap(varmap_list, measure, global_threshold=False):
    "returns variation map of all variation maps specified by the measure"
    if measure == 'mean':
        varmap = get_meanmap(varmap_list)
        if global_threshold:
            varmap.fill(np.mean(varmap))
            print(":: global threshold: "+str(np.mean(varmap)))
    elif measure == 'rmsd':
        varmap = get_rmsdmap(varmap_list)
        if global_threshold:
            varmap.fill(np.mean(varmap))
            print(":: global threshold: "+str(np.mean(varmap)))
    else:
        varmap = get_maxmap(varmap_list)
        if global_threshold:
            print(":: global threshold: "+str(varmap.max()))
            varmap.fill(varmap.max())
    return varmap
    

if __name__ == '__main__':
    no_args = len(sys.argv)
    usage = 'usage: %prog [options] selection_dir'
    parser = OptionParser(usage=usage)
    parser.add_option("-m", "--measure", 
            default="max", 
            help="is the measure to determine the threshold: max (default), mean.")
    parser.add_option("-g", "--global",
            action="store_true", dest="global_threshold", default=False,
            help='instead of a local threshold one global threshold value is used.')
    parser.add_option("-o", "--output_file", 
            default="varmap.mrc", 
            help="is the filename that the variation map is written to")
    (options, args) = parser.parse_args()
    if len(args) == 0:
        selection_dir = os.getcwd()
    else:
        selection_dir = args[0]
    if not os.path.exists(selection_dir):
        sys.exit('The specified selection directory '+selection_dir+' does not exist.')
    map_list = list_selection_maps(selection_dir)
    map_pairs = get_map_pairs(map_list)
    map_pairs = remove_mismatched_pairs(map_pairs)
    varmap_list = get_variationmaps(map_pairs)
    varmap = get_varmap(varmap_list, options.measure, options.global_threshold)
    mrc_image = get_mrc_image(map_list[0])
    #varmap_output_file =  os.path.join(selection_dir, options.output_file) 
    save_mrc_image(varmap, mrc_image, options.output_file)
    #plotImage(varmap)
    #plt.show()
    
