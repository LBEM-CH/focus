#!/bin/sh
# change_dylibs
if [ $# -lt 1 ]
then
	echo "No build directory specified"
	echo "Usage: `basename $0:` <build_dir>" >&2
	echo 'Aborting!'
	exit 1
fi

build_dir=$1
echo "the build dir is $1"
apps="2dx_image/2dx_image.app/Contents/PlugIns/imageformats 2dx_merge/2dx_merge.app/Contents/PlugIns/imageformats"
for loop in $apps
do
        path=$build_dir"/"$loop	
	echo "in $path"
	for loop in `ls $path`
	do
		file=$path"/"$loop
		echo "changing the dylibs of" $loop
		echo "with absolute path" $file
		install_name_tool -change QtGui.framework/Versions/4/QtGui @executable_path/../Frameworks/QtGui.framework/Versions/4/QtGui $file 	
		install_name_tool -change QtCore.framework/Versions/4/QtCore @executable_path/../Frameworks/QtCore.framework/Versions/4/QtCore $file 
		otool -L $file
		if [ $loop = "libqsvg.dylib" ] ; then
			echo "removing $file"
			rm $file
		fi
	done
done
fortran_bin="kernel/mrc/bin"
path="$build_dir/$fortran_bin"
echo "chaning binaries in $path" 
for exe in `ls $path`
do
	file="$path/$exe"
	echo "changing the dylibs of $file"
	install_name_tool -change /opt/local/lib/libfftw3f.3.dylib @executable_path/../lib/libfftw3f.3.dylib $file
#	install_name_tool -change /usr/local/lib/libgfortran.3.dylib @executable_path/../lib/libgfortran.3.dylib $exe 
	otool -L $file 
done
