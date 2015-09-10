#!/bin/bash
#
# scirpt to package the 2dx installer with package maker 
#
# Marcel Arheit  21.01.2011

if [ $# -lt 3 ]
then
	echo "Not enough arguments specified"
	echo "Usage: `basename $0:` <root_dir> <package_dir> <version>" >&2
	echo 'Aborting!'
	exit 1
fi
DIR=`dirname $0`
echo "MAC OSX INSALLATION FILES DIR=$DIR"
ROOT=$1
PACKAGE_DIR=$2
VERSION=$3

PACKAGE_2dx=`echo "${PACKAGE_DIR}/2dx-${VERSION}.pkg"`
PACKAGE_2dx_main=${PACKAGE_DIR}/2dx_main.pkg
PACKAGE_2dx_merge=${PACKAGE_DIR}/2dx_merge.pkg
PACKAGE_2dx_image=${PACKAGE_DIR}/2dx_image.pkg
PACKAGE_2dx_logbrowser=${PACKAGE_DIR}/2dx_logbrowser.pkg

if [ -d $PACKAGE_DIR ]
then
	echo "removing previous version in $PACKAGE_DIR" 
	rm -r $PACKAGE_DIR
        mkdir $PACKAGE_DIR
else
        mkdir -p $PACKAGE_DIR
fi

echo "The contents are:"
ls $ROOT

#packagemaker \
#--title "2dx Nightly Build" \
#--version  DATE=`date "+%d_%b_%Y"` \
#--filter "\.DS_Store" \
#--resources $DIR/Resources/ \
#--scripts $DIR/Resources/scripts/ \
#--root-volume-only \
#--domain system \
#--verbose \
#--no-relocate \
#--install-to "/opt/2dx" \
#--id org.2dx.pkg \
#--root $ROOT \
#--out $OUTFILE \
#--discard-forks \
#--verbose

pkgbuild \
    --root ${ROOT} \
    --identifier "org.cina.pkg.2dx_main" \
    --version ${VERSION} \
    --install-location "/opt/2dx" \
    --filter "\.DS_Store" \
    ${PACKAGE_2dx_main}

pkgbuild \
    --root ${ROOT}/2dx_merge \
    --identifier "org.cina.pkg.2dx_merge" \
    --version ${VERSION} \
    --install-location "/opt/2dx" \
    ${PACKAGE_2dx_merge}

pkgbuild \
    --root ${ROOT}/2dx_image \
    --identifier "org.cina.pkg.2dx_image" \
    --version ${VERSION} \
    --install-location "/opt/2dx" \
    ${PACKAGE_2dx_image}

pkgbuild \
    --root ${ROOT}/2dx_logbrowser \
    --identifier "org.cina.pkg.2dx_logbrowser" \
    --version ${VERSION} \
    --install-location "/opt/2dx" \
    ${PACKAGE_2dx_logbrowser}

productbuild \
    --distribution ${DIR}/Distribution.xml \
    --package-path ${PACKAGE_DIR} \
    --resources $DIR/Resources/ \
    ${PACKAGE_2dx}
    