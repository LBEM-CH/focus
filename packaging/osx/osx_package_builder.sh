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
PARENT_DIR=`dirname $0`
echo "MAC OSX INSALLATION FILES DIR=$PARENT_DIR"
ROOT=$1
PACKAGE_DIR=$2
VERSION=$3

PACKAGE_2dx_main=${PACKAGE_DIR}/2dx_main.pkg
PACKAGE_2dx_merge=${PACKAGE_DIR}/2dx_merge.pkg
PACKAGE_2dx_image=${PACKAGE_DIR}/2dx_image.pkg
PACKAGE_2dx_logbrowser=${PACKAGE_DIR}/2dx_logbrowser.pkg

PRODUCT_FOLDER=`echo "${PACKAGE_DIR}/2dx-${VERSION}"`
PRODUCT_PKG=`echo "2dx-${VERSION}.pkg"`
PRODUCT_DMG=`echo "2dx-${VERSION}.dmg"`
PRODUCT_VOLNAME=`echo "2dx-${VERSION}"`

echo '*############################################################################*'
echo '| Preparing                                                                  |'
echo '*============================================================================*'
echo '|                                                                            |'
echo "SUPPLIED VERSION: ${VERSION}"
echo "SUPPLIED PACKAGE DIRECTORY: ${PACKAGE_DIR}"
echo ''
echo "WILL CREATE PRODUCT IN: $PRODUCT_FOLDER"
echo "PRODUCT PAKCAGE FILE WILL BE: $PRODUCT_PKG"
echo "PRODUCT DMG FILE WILL BE: $PRODUCT_DMG"
echo "PRODUCT VOLNAME FILE WILL BE: $PRODUCT_VOLNAME"
echo '|                                                                            |'
echo '*============================================================================*'
echo ''
echo ''
echo ''
echo ''

if [ -d $PACKAGE_DIR ]
then
	echo "removing previous version in $PACKAGE_DIR" 
	rm -r $PACKAGE_DIR
        mkdir $PACKAGE_DIR
else
        mkdir -p $PACKAGE_DIR
fi

echo '*############################################################################*'
echo '| The contents of package will be:                                           |'
echo "| ($ROOT})"
echo '*============================================================================*'
echo '|                                                                            |'           
ls $ROOT
echo '|                                                                            |'
echo '*============================================================================*'
echo ''
echo ''
echo ''
echo ''
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

echo '*############################################################################*'
echo '| Building the dependent packages                                            |'
echo '*============================================================================*'
echo '|                                                                            |'
pkgbuild \
    --root ${ROOT} \
    --scripts $PARENT_DIR/scripts/ \
    --identifier "org.cina.pkg.2dx_main" \
    --version ${VERSION} \
    --install-location "/Applications/2dx" \
    --filter "\.DS_Store" \
    ${PACKAGE_2dx_main}

pkgbuild \
    --root ${ROOT}/2dx_merge \
    --identifier "org.cina.pkg.2dx_merge" \
    --version ${VERSION} \
    --install-location "/Applications/2dx/2dx_merge" \
    ${PACKAGE_2dx_merge}

pkgbuild \
    --root ${ROOT}/2dx_image \
    --identifier "org.cina.pkg.2dx_image" \
    --version ${VERSION} \
    --install-location "/Applications/2dx/2dx_image" \
    ${PACKAGE_2dx_image}

pkgbuild \
    --root ${ROOT}/2dx_logbrowser \
    --identifier "org.cina.pkg.2dx_logbrowser" \
    --version ${VERSION} \
    --install-location "/Applications/2dx/2dx_logbrowser" \
    ${PACKAGE_2dx_logbrowser}
echo '|                                                                            |'
echo '*============================================================================*'
echo ''
echo ''
echo ''
echo ''

echo '*############################################################################*'
echo '| Building the main product package                                          |'
echo '*============================================================================*'
echo '|                                                                            |'
productbuild \
    --distribution ${PARENT_DIR}/Distribution.xml \
    --package-path ${PACKAGE_DIR} \
    --resources $PARENT_DIR/resources/ \
    ${PACKAGE_DIR}/${PRODUCT_PKG}
    
echo '|                                                                            |'
echo '*============================================================================*'
echo ''
echo ''
echo ''
echo ''

echo '*############################################################################*'
echo '| Compressing to disk Image                                                  |'
echo '*============================================================================*'
echo '|                                                                            |'
mkdir $PRODUCT_FOLDER
cp ${PACKAGE_DIR}/${PRODUCT_PKG} ${PRODUCT_FOLDER}
hdiutil create \
  -volname ${PRODUCT_VOLNAME} \
  -srcfolder ${PRODUCT_FOLDER} \
  -ov \
  ${PACKAGE_DIR}/${PRODUCT_DMG}
    
echo '|                                                                            |'
echo '*============================================================================*'
echo ''
echo ''
echo ''
echo ''
