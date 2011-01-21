#!/bin/bash
#
# scirpt to upload a file on the 2dx.org website 
#
# Marcel Arheit 21.01.2011

#These two global variables should be changed to fit the local setup
#Attention: When altering BUILD_DIR this has to be altered in installation/mac_osx_installer/installer_nightly_build.pmdoc as well 
URL=http://www.2dx.unibas.ch/download/2dx-software/2dx-installer/nightly-build
echo '*############################################################################*'
echo '| Locating the file                                                          |'
echo '*============================================================================*'
echo '|                                                                            |'

if [ $# -lt 1 ]
then
	echo "No file specified"
	echo "Usage: `basename $0:` <file>" >&2
	echo 'Aborting!'
	exit 1
fi
FILE=$1

if [ ! -f $FILE ]; then
	"Could not find file $FILE"
	exit 2;
fi

echo '|                                                                            |'
echo '*============================================================================*'
echo ''
echo ''
echo ''
echo ''






echo '*############################################################################*'
echo '| Uploading the file                                                         |'
echo '*============================================================================*'
echo '|                                                                            |'

echo "Uploading $FILE to $URL"
cadaver $URL <<EOT
put $FILE 
quit
EOT
echo "cadaver $URL <<EOT
put $FILE 
quit
EOT"


#echo svn up || (echo "Could not update the repository"; exit 3)
#svn up || (echo "Could not update the repository"; exit 3)
#if [ $? -gt 0 ]; then
#	exit 3;
#fi
echo '|                                                                            |'
echo '*============================================================================*'
echo ''
echo ''
echo ''
echo ''

exit 0
