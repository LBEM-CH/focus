from EMAN2 import *
from sparx import *

import os

from joblib import Parallel, delayed


def process(name):
	old_path = os.getcwd()
	print old_path
	os.chdir(old_path + "/../" + name)
	command = "2dx_image " + old_path + "/../" + name + " '" + '"2dx_unbend_movie"' + "'"
	#command = "2dx_image " + old_path + "/../" + name + " '" + '"2dx_unbend2"' + "'"
	print command 
	os.system(command)
	command = "2dx_image " + old_path + "/../" + name + " '" + '"2dx_applyCTF"' + "'"
	print command 
	os.system(command)
	command = "2dx_image " + old_path + "/../" + name + " '" + '"2dx_generateMAP"' + "'"
	print command 
	os.system(command)
	os.chdir(old_path)

if __name__ == "__main__":
	
	image_list = []
	for i in range(1,len(sys.argv)):
		image_list.append(sys.argv[i])
	
	Parallel(n_jobs=8)(delayed(process)(i) for i in image_list)
	#[process(i) for i in image_list]
