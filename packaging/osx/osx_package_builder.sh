#!/bin/bash
#
# script to package the focus installer with package maker 
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

PRODUCT_FOLDER=`echo "${PACKAGE_DIR}/focus-${VERSION}"`
PRODUCT_PKG=`echo "focus-${VERSION}.pkg"`
PRODUCT_DMG=`echo "focus-${VERSION}.dmg"`
PRODUCT_VOLNAME=`echo "focus-${VERSION}"`

echo '*############################################################################*'
echo '| Preparing                                                                  |'
echo '*============================================================================*'
echo '|                                                                            |'
echo "SUPPLIED VERSION: ${VERSION}"
echo "SUPPLIED PACKAGE DIRECTORY: ${PACKAGE_DIR}"
echo ''
echo "WILL CREATE PRODUCT IN: $PRODUCT_FOLDER"
echo "PRODUCT DMG FILE WILL BE: $PRODUCT_DMG"
echo "PRODUCT VOLNAME FILE WILL BE: $PRODUCT_VOLNAME"

if [ -d $PACKAGE_DIR ]
then
	echo "Removing previous version in $PACKAGE_DIR" 
	rm -r $PACKAGE_DIR
        mkdir $PACKAGE_DIR
else
        mkdir -p $PACKAGE_DIR
fi
echo '|                                                                            |'
echo '*============================================================================*'
echo ''
echo ''
echo ''
echo ''

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


echo '*############################################################################*'
echo '| Getting and reapiring component list                                       |'
echo '*============================================================================*'
echo '|                                                                            |'
pkgbuild \
    --analyze \
    --root ${ROOT} \
    ${PACKAGE_DIR}/focus.plist 

echo "Making packages non relocatable"

for i in `fgrep -n "<key>BundleIsRelocatable</key>" ${PACKAGE_DIR}/focus.plist | cut -d':' -f 1`
do
    i=$((i + 1))
    echo changing line $i
    sed -i -e "${i}s|<true/>|<false/>|" ${PACKAGE_DIR}/focus.plist
done

echo '|                                                                            |'
echo '*============================================================================*'
echo ''
echo ''
echo ''
echo ''


echo '*############################################################################*'
echo '| Building the dependent packages                                            |'
echo '*============================================================================*'
echo '|                                                                            |'
pkgbuild \
    --root ${ROOT} \
    --component-plist ${PACKAGE_DIR}/focus.plist \
    --scripts $PARENT_DIR/scripts/ \
    --identifier "org.cina.pkg.focus" \
    --version ${VERSION} \
    --install-location "/Applications/focus" \
    --filter "\.DS_Store" \
    ${PACKAGE_DIR}/focus.pkg

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
  -size 100m \
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
