import sys
import os
from optparse import OptionParser
from utilities import *
from plotting import *
from scipy import stats

def read_images_from_list(filelist):
    "reads mrc images from file list"
    mrc_list = [ getImage(f) for f in filelist ]
    return mrc_list 


def list_files(dir1, dir2):
    "reads files in the directories of both conformations"
    file_list1 = list_files_in_dir(dir1,".mrc")
    file_list2 = list_files_in_dir(dir2,".mrc")
    return [file_list1, file_list2]

def read_images(file_list1, file_list2):
    "reads the images from the file lists and retunrs them as an array list"
    mrc_list1 = read_images_from_list(file_list1)
    mrc_list2 = read_images_from_list(file_list2)
    return [mrc_list1, mrc_list2] 
    
def get_conformation_array(image_list):
    "converts list of images to an numpy array"
    conformation_array = np.array(image_list)
    return conformation_array

def determine_significance(conformation1, conformation2, confidence=99.0, method="student"):
    if method == "welch":
        equal_variances = False
    else:
        equal_variances = True
    p_threshold = (100.0-confidence)/100.0
    [t, p] = stats.ttest_ind(conformation1, conformation2, equal_var = equal_variances)
    significance = p <= p_threshold
    return significance

def significance2varmap(significance):
    """docstring for significance2varmap"""
    return 1 - significance 

def save_varmap(varmap, mrc, output_file):
    "saves the map to the specified output file as an mrc file"
    mrc.setImage(varmap)
    mrc.tofile(output_file)
    


if __name__ == '__main__':
    no_args = len(sys.argv)
    usage = 'usage: %prog [options] maps_dir1 maps_dir2'
    parser = OptionParser(usage=usage)
    parser.add_option("-m", "--method", 
            default="student", 
            help="method used to determine the signifincance of the conformational differences: t-test or welch")
    parser.add_option("-c", "--confidence",
            type=float, default=99.0,
            help='instead of a local threshold one global threshold value is used.')
    parser.add_option("-o", "--output_file", 
            default="varmap.mrc", 
            help="is the filename that the variation map is written to")
    parser.add_option("-d", "--debug",
            action="store_true", default=False,
            help='flag to visually debug the treshold')
    (options, args) = parser.parse_args()
    if len(args) < 2:
        parser.error("you have to specify the directories for both conformations where all the individual maps lie.")
    maps_dir1 = args[0]
    maps_dir2 = args[1]
    if not os.path.exists(maps_dir1):
        sys.exit('The specified maps directory '+map_dir1+' does not exist.')
    if not os.path.exists(maps_dir2):
        sys.exit('The specified maps directory '+map_dir2+' does not exist.')
    [file_list1, file_list2] = list_files(maps_dir1, maps_dir2)
    [images1, images2] = read_images(file_list1, file_list2)
    conformation1 = get_conformation_array(images1)
    conformation2 = get_conformation_array(images2)
    significance = determine_significance(conformation1, conformation2, options.confidence,options.method)
    varmap =  significance2varmap(significance)
    if options.debug:
        plotImage(varmap)
        #plt.show()
    #TODO: hack to get header right
    mrc_image = get_mrc_image(file_list1[0])
    #mrc_image.mode = 0
    save_mrc_image(varmap, mrc_image, options.output_file)
    if options.debug:
        output = get_mrc_image(options.output_file)
        plotMRCImage(output)
        plt.show()
    
     
