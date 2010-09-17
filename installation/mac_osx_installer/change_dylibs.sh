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
		otool -L $loop
		if [ "$loop" = "libqsvg.dylib" ] ; then
			echo "removing $loop"
			rm $loop
		fi
	done
	# change back to build dir
	cd $build_dir
done
cd kernel/mrc/bin
echo "in `pwd`"
for exe in `ls`
do
	echo "changing the dylibs of" $exe
	install_name_tool -change /opt/local/lib/libfftw3f.3.dylib @executable_path/../../../lib/libfftw3f.3.dylib $exe 
	install_name_tool -change /usr/local/lib/libgfortran.3.dylib @executable_path/../../../lib/libgfortran.3.dylib $exe 
	otool -L $exe
done
