import sys
import os

if(len(sys.argv) < 2):
    print "Usage:\npython " + sys.argv[0] + " <2DX Source Code Directory>"
    exit(0);

SOURCE_CODE_DIR = sys.argv[1];
SCRIPTS_DIR = SOURCE_CODE_DIR + "/scripts/"
CONFIG_FILE = SOURCE_CODE_DIR + "/apps/resources/config/2dx_master.cfg"

cfgfile = open(CONFIG_FILE, 'r')

paramsMasterList = []
for line in cfgfile:
    if line.startswith("set"):
        line = line[4:]
        paramsMasterList.append(line.split("=")[0].strip())

cfgfile.close()

print "Total number of parameters in master.cfg: " + str(len(paramsMasterList))

paramsUsedList = []
imageParams = []
projectParams = []
for (dirpath, dirnames, filenames) in os.walk(SCRIPTS_DIR ):
    for scriptFileName in filenames:
        if(scriptFileName.endswith(".script")):
            scriptFile = open(dirpath + '/' +scriptFileName, 'r')
            
            for line in scriptFile:
                if line.startswith("#$end_vars"):
                    break
                if line.startswith("set"):
                    line = line[4:]
                    var = line.split("=")[0].strip()
                    if (dirpath.find("image") >= 0):
                        if var not in imageParams:
                            imageParams.append(var)
                    else:
                        if var not in projectParams:
                            projectParams.append(var)
                    if var not in paramsUsedList:
                        paramsUsedList.append(var)
                        
            scriptFile.close()

print "Total number of parameters used in scripts: " + str(len(paramsUsedList))
print ""

paramsNotUsed = []
for var in paramsMasterList:
    if var not in paramsUsedList:
        paramsNotUsed.append(var)

print "Parameters not used at all: " + str(len(paramsNotUsed))
print paramsNotUsed
print ""

print "Parameters used in Images: " + str(len(imageParams))
print imageParams
print ""

print "Parameters used in Merge/Project level scripts: " + str(len(projectParams))
print projectParams
print ""

paramsOnlyInImage = []
paramsOnlyInProject = []
paramsUsedInBoth = []
for var in paramsUsedList:
    if (var in imageParams) and (var not in projectParams):
        paramsOnlyInImage.append(var)
    elif (var in projectParams) and (var not in imageParams):
        paramsOnlyInProject.append(var)
    elif (var in imageParams) and (var in projectParams):
        paramsUsedInBoth.append(var)

print "Parameters used only in Images and not in Project/Merge level: " + str(len(paramsOnlyInImage))
print paramsOnlyInImage
print ""

print "Parameters used only in Merge/Project level scripts and not in images: " + str(len(paramsOnlyInProject))
print paramsOnlyInProject
print ""

print "Parameters used in both Image level and Merge/Project level: " + str(len(paramsUsedInBoth))
print paramsUsedInBoth
print ""
