#!/bin/sh
# change_dylibs
build_dir=$1
echo "the build dir is $1"
cd $build_dir
apps=" 2dx_image/2dx_image.app/Contents/PlugIns/imageformats 2dx_merge/2dx_merge.app/Contents/PlugIns/imageformats"
for loop in $apps
do
	cd $loop
	echo "in $loop" 
	for loop in `ls`
	do
		echo "changing the dylibs of" $loop
		install_name_tool -change QtGui.framework/Versions/4/QtGui @executable_path/../Frameworks/QtGui.framework/Versions/4/QtGui $loop 	
		install_name_tool -change QtCore.framework/Versions/4/QtCore @executable_path/../Frameworks/QtCore.framework/Versions/4/QtCore $loop
		#if test "$loop" -eq "libqsvg.dylib" ; then
		#	echo "additional dylibs have to be changed for $loop"
			install_name_tool -change QtSvg.framework/Versions/4/QtSvg @executable_path/../Frameworks/QtSvg.framework/Versions/4/QtSvg $loop 	
			install_name_tool -change QtXml.framework/Versions/4/QtXml @executable_path/../Frameworks/QtXml.framework/Versions/4/QtXml $loop
		#fi
		otool -L $loop
	done
	# change back to build dir
	cd $build_dir
done
