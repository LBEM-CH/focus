#!/bin/sh
# change_dylibs
#
# scirpt to change the dylibs of the 2dx and MRC binaries
#
# Marcel Arheit


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

if [ -f /opt/local/lib/libfftw3f.3.dylib ]; then
	FFTW_LIB=/opt/local/lib/libfftw3f.3.dylib
	echo "Found FFTW in $FFTW_LIB"
else
       if [ -f /usr/local/lib/libfftw3f.3.dylib ]; then
		FFTW_LIB=/usr/local/lib/libfftw3f.3.dylib
		echo "Found FFTW in $FFTW_LIB"
	else
		FFTW_LIB=NOT_FOUND
		echo "FFTW not FOUND!"
	       	exit 2
	fi
fi


if [ -f /opt/local/lib/libfftw3f_threads.3.dylib ]; then
	FFTW_LIB_THREAD=/opt/local/lib/libfftw3f_threads.3.dylib
	echo "Found FFTW-threaded in $FFTW_LIB_THREAD"
else
       if [ -f /usr/local/lib/libfftw3f_threads.3.dylib ]; then
		FFTW_LIB_THREAD=/usr/local/lib/libfftw3f_threads.3.dylib
		echo "Found FFTW-threaded in $FFTW_LIB_THREAD"
	else
		FFTW_LIB_THREAD=NOT_FOUND
		echo "FFTW-threaded not FOUND!"
	       	exit 2
	fi
fi


binaries="2dx_image/2dx_image.app/Contents/MacOS 2dx_merge/2dx_merge.app/Contents/MacOS kernel/mrc/lib"
for loop in $binaries
do
	echo "cp $FFTW_LIB  $build_dir/$loop"
	cp $FFTW_LIB $build_dir/$loop
	echo "cp $FFTW_LIB_THREAD  $build_dir/$loop"
	cp $FFTW_LIB_THREAD $build_dir/$loop	
done
fortran_bin="kernel/mrc/bin"
path="$build_dir/$fortran_bin"
echo "chaning binaries in $path" 
for exe in `ls $path`
do
	file="$path/$exe"
	echo "changing the dylibs of $file"
	install_name_tool -change $FFTW_LIB @executable_path/../lib/libfftw3f.3.dylib $file
	install_name_tool -change $FFTW_LIB_THREAD @executable_path/../lib/libfftw3f_threads.3.dylib $file
	#	install_name_tool -change /usr/local/lib/libgfortran.3.dylib @executable_path/../lib/libgfortran.3.dylib $file 
	otool -L $file 
done

executables="2dx_image/2dx_image.app/Contents/MacOS/2dx_image 2dx_merge/2dx_merge.app/Contents/MacOS/2dx_merge"
for exe in $executables 
do
	file="$build_dir/$exe"
	echo "changing the dylibs of $file"
	install_name_tool -change $FFTW_LIB @executable_path/libfftw3f.3.dylib $file
	install_name_tool -change $FFTW_LIB_THREAD @executable_path/libfftw3f_threads.3.dylib $file
	otool -L $file 
done
