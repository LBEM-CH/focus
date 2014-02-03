from EMAN2  import *
from sparx  import *

import os
import shutil


if __name__ == '__main__':

	if len(sys.argv) != 2:
		sys.exit("Usage: python folder_name [foldername]")

	folder_name = "../" + sys.argv[1]

	print folder_name
	
	mrc_file_name = folder_name.split("/")[0] + "/" + folder_name.split("/")[1] + "/" + folder_name.split("/")[2] + "/m" + folder_name.split("/")[2] + ".mrc"
	cc_file_name = folder_name.split("/")[0] + "/" + folder_name.split("/")[1] + "/" + folder_name.split("/")[2] + "/cc_profile.dat"
	config_file_name = folder_name.split("/")[0] + "/" + folder_name.split("/")[1] + "/" + folder_name.split("/")[2] + "/2dx_image.cfg"
	
	
	print "\tmrc file:", mrc_file_name
	print "\tcc file:", cc_file_name
	print "\tconfig file:", config_file_name
	
	n = len(os.listdir("DATA4SPARX")) / 3
	
	print "\timage number:", n
	
	shutil.copy2(mrc_file_name, "DATA4SPARX/image_" + str(n) + ".mrc")
	shutil.copy2(cc_file_name, "DATA4SPARX/profile_" + str(n) + ".dat")
	shutil.copy2(config_file_name, "DATA4SPARX/config_" + str(n) + ".ctf")
	
	
	
