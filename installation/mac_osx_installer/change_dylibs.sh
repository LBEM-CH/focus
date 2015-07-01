#!/bin/sh
# change_dylibs
#
# scirpt to change the dylibs of the 2dx and MRC binaries
#
# Marcel Arheit
# 
# Adapted by Sebastian Scherer
#
#
# How to add antoher library 
# 1. Add if statement 
# 2. Copy Lib to build_dir/loop
# 3. install_name_tool for Lib


if [ $# -lt 1 ]
then
	echo "No build directory specified"
	echo "Usage: `basename $0:` <build_dir>" >&2
	echo 'Aborting!'
	exit 1
fi

build_dir=$1
echo "the build dir is $1"
if [ -f /opt/local/lib/libfftw3.3.dylib ]; then
	FFTW_LIB=/opt/local/lib/libfftw3.3.dylib
	echo "Found FFTW in $FFTW_LIB"
else
       if [ -f /usr/local/lib/libfftw3.3.dylib ]; then
		FFTW_LIB=/usr/local/lib/libfftw3.3.dylib
		echo "Found FFTW in $FFTW_LIB"
	else
		FFTW_LIB=NOT_FOUND
		echo "FFTW not FOUND!"
	       	exit 2
	fi
fi

if [ -f /opt/local/lib/libfftw3f.3.dylib ]; then
	FFTWF_LIB=/opt/local/lib/libfftw3f.3.dylib
	echo "Found FFTWF in $FFTWF_LIB"
else
       if [ -f /usr/local/lib/libfftw3f.3.dylib ]; then
		FFTWF_LIB=/usr/local/lib/libfftw3f.3.dylib
		echo "Found FFTWF in $FFTWF_LIB"
	else
		FFTWF_LIB=NOT_FOUND
		echo "FFTWF not FOUND!"
	       	exit 2
	fi
fi

if [ -f /opt/local/lib/libfftw3_threads.3.dylib ]; then
	FFTW_LIB_THREAD=/opt/local/lib/libfftw3_threads.3.dylib
	echo "Found FFTW-threaded in $FFTW_LIB_THREAD"
else
       if [ -f /usr/local/lib/libfftw3_threads.3.dylib ]; then
		FFTW_LIB_THREAD=/usr/local/lib/libfftw3_threads.3.dylib
		echo "Found FFTW-threaded in $FFTW_LIB_THREAD"
	else
		FFTW_LIB_THREAD=NOT_FOUND
		echo "FFTW-threaded not FOUND!"
	       	exit 2
	fi
fi

if [ -f /opt/local/lib/libfftw3f_threads.3.dylib ]; then
	FFTWF_LIB_THREAD=/opt/local/lib/libfftw3f_threads.3.dylib
	echo "Found FFTWF-threaded in $FFTWF_LIB_THREAD"
else
       if [ -f /usr/local/lib/libfftw3f_threads.3.dylib ]; then
		FFTWF_LIB_THREAD=/usr/local/lib/libfftw3f_threads.3.dylib
		echo "Found FFTWF-threaded in $FFTWF_LIB_THREAD"
	else
		FFTWF_LIB_THREAD=NOT_FOUND
		echo "FFTWF-threaded not FOUND!"
	       	exit 2
	fi
fi

# BUILD SYSTEM DEPENDENT, FIX IT LATER
if [ -f /opt/local/lib/libgcc/libgfortran.3.dylib ]; then
	GFORTRAN_LIB=/opt/local/lib/libgcc/libgfortran.3.dylib
	echo "Found gfortran in $GFORTRAN_LIB"
else
       if [ -f /usr/local/lib/libgfortran.3.dylib ]; then
		GFORTRAN_LIB=/usr/local/lib/libgfortran.3.dylib
		echo "Found gfortran in $GFORTRAN_LIB"
	else
		GFORTRAN_LIB=NOT_FOUND
		echo "gfortran not FOUND!"
	       	exit 2
	fi
fi

if [ -f /opt/local/lib/libgcc/libquadmath.0.dylib ]; then
	QUADMATH_LIB=/opt/local/lib/libgcc/libquadmath.0.dylib
	echo "Found quadmath in $QUADMATH_LIB"
else
       if [ -f /usr/local/lib/libquadmath.0.dylib  ]; then
		QUADMATH_LIB=/usr/local/lib/libquadmath.0.dylib
		echo "Found quadmath in $QUADMATH_LIB"
	else
		QUADMATH_LIB=NOT_FOUND
		echo "quadmath not FOUND!"
	       	exit 2
	fi
fi

if [ -f /opt/local/lib/libgcc/libstdc++.6.dylib ]; then
	CPP_LIB=/opt/local/lib/libgcc/libstdc++.6.dylib
	echo "Found libstdc++.6.dylib in $CPP_LIB"
else
       if [ -f /usr/local/lib/gcc48/libstdc++.6.dylib ]; then
		CPP_LIB=/usr/local/lib/gcc48/libstdc++.6.dylib
		echo "Found libstdc++.6.dylib in $CPP_LIB"
	else
		CPP_LIB=NOT_FOUND
		echo "libstdc++ not FOUND!"
	       	exit 2
	fi
fi

if [ -f /opt/local/lib/libgcc/libgcc_s.1.dylib ]; then
	GCC_LIB=/opt/local/lib/libgcc/libgcc_s.1.dylib
	echo "Found libgcc_s.1.dylib in $CPP_LIB"
else
       if [ -f /usr/local/lib/gcc48/libgcc_s.1.dylib ]; then
		CPP_LIB=/usr/local/lib/gcc48/libgcc_s.1.dylib
		echo "Found libgcc_s.1.dylib in $CPP_LIB"
	else
		CPP_LIB=NOT_FOUND
		echo "libgcc_s.1.dylib not FOUND!"
	       	exit 2
	fi
fi

# omp-lib
if [ -f /opt/local/lib/libgcc/libgomp.1.dylib ]; then
	OMP_LIB=/opt/local/lib/libgcc/libgomp.1.dylib
	echo "Found libgomp.1.dylib in $OMP_LIB"
else
       if [ -f /usr/local/lib/gcc48/libgomp.1.dylib ]; then
		OMP_LIB=/usr/local/lib/gcc48/libgomp.1.dylib
		echo "Found libgomp.1.dylib in $OMP_LIB"
	else
		OMP_LIB=NOT_FOUND
		echo "libgomp.1.dylib not FOUND!"
	       	exit 2
	fi
fi


# sys-lib
if [ -f  /usr/lib/libSystem.B.dylib ]; then
    SYS_LIB=/usr/lib/libSystem.B.dylib
    echo "Found libSystem.B.dylib in $SYS_LIB"
fi

binaries="kernel/mrc/lib"
for loop in $binaries
do
    echo "cp $FFTW_LIB  $build_dir/$loop"
    cp $FFTW_LIB $build_dir/$loop
    echo "cp $FFTWF_LIB  $build_dir/$loop"
    cp $FFTWF_LIB $build_dir/$loop
    echo "cp $FFTW_LIB_THREAD  $build_dir/$loop"
    cp $FFTW_LIB_THREAD $build_dir/$loop
    echo "cp $FFTWF_LIB_THREAD  $build_dir/$loop"
    cp $FFTWF_LIB_THREAD $build_dir/$loop	
    echo "cp $GFORTRAN_LIB $build_dir/$loop"
    cp $GFORTRAN_LIB $build_dir/$loop
    echo "cp $QUADMATH_LIB $build_dir/$loop"
    cp $QUADMATH_LIB $build_dir/$loop
    echo "cp $CPP_LIB $build_dir/$loop"
    cp $CPP_LIB $build_dir/$loop
    echo "cp $GCC_LIB $build_dir/$loop"
    cp $GCC_LIB $build_dir/$loop
    echo "cp $OMP_LIB $build_dir/$loop"
    cp $OMP_LIB $build_dir/$loop
    echo "cp $SYS_LIB $build_dir/$loop"
    cp $SYS_LIB $build_dir/$loop
done

#mkdir $build_dir/kernel/mrc/lib/system/
#for sys_lib in `ls /usr/lib/system/*.dylib`
#do
#cp $sys_lib $build_dir/kernel/mrc/lib/system/
#done

#for loop in `ls /usr/lib/system/`
#do
#echo $loop
#echo "install_name_tool -change /usr/lib/system/$loop @executable_path/../lib/system/$loop $build_dir/kernel/mrc/lib/libSystem.B.dylib"
#install_name_tool -change /usr/lib/system/$loop @executable_path/../lib/system/$loop $build_dir/kernel/mrc/lib/libSystem.B.dylib
#done

#for loop1 in `ls /usr/lib/system/`
#do
#    for loop2 in `ls /usr/lib/system/`
#    do
#        echo "install_name_tool -change /usr/lib/system/$loop2 @executable_path/../lib/system/$loop2 $build_dir/kernel/mrc/lib/system/$loop1"
#        install_name_tool -change /usr/lib/system/$loop2 @executable_path/../lib/system/$loop2 $build_dir/kernel/mrc/lib/system/$loop1
#    done
#    otool -L $build_dir/kernel/mrc/lib/system/$loop1
#done

fortran_bin="kernel/mrc/bin"
path="$build_dir/$fortran_bin"
lib_path="$build_dir/kernel/mrc/lib"
target_lib_path="/opt/2dx/kernel/mrc/lib"


echo "install_name_tool -change $QUADMATH_LIB  $target_lib_path/libquadmath.0.dylib $lib_path/libgfortran.3.dylib"
install_name_tool -change $QUADMATH_LIB  $target_lib_path/libquadmath.0.dylib $lib_path/libgfortran.3.dylib

echo "chaning binaries in $path" 
for exe in `ls $path`
do
	file="$path/$exe"
	echo "changing the dylibs of $file"
#otool -L $file
	echo "changed otool command:"
        install_name_tool -change $FFTW_LIB @executable_path/../lib/libfftw3.3.dylib $file
	install_name_tool -change $FFTWF_LIB @executable_path/../lib/libfftw3f.3.dylib $file
        install_name_tool -change $FFTW_LIB_THREAD @executable_path/../lib/libfftw3_threads.3.dylib $file
	install_name_tool -change $FFTWF_LIB_THREAD @executable_path/../lib/libfftw3f_threads.3.dylib $file
	install_name_tool -change $GFORTRAN_LIB  @executable_path/../lib/libgfortran.3.dylib $file 
	install_name_tool -change $QUADMATH_LIB @executable_path/../lib/libquadmath.0.dylib  $file
	install_name_tool -change $CPP_LIB @executable_path/../lib/libstdc++.6.dylib  $file
    install_name_tool -change $GCC_LIB @executable_path/../lib/libgcc_s.1.dylib  $file
	install_name_tool -change $OMP_LIB @executable_path/../lib/libgomp.1.dylib  $file
    install_name_tool -change $SYS_LIB @executable_path/../lib/libSystem.B.dylib  $file
	otool -L $file 
done

for loop in $build_dir/$binaries/*.dylib
do
install_name_tool -change $FFTW_LIB @executable_path/../lib/libfftw3.3.dylib $loop
install_name_tool -change $FFTWF_LIB @executable_path/../lib/libfftw3f.3.dylib $loop
install_name_tool -change $FFTW_LIB_THREAD @executable_path/../lib/libfftw3_threads.3.dylib $loop
install_name_tool -change $FFTWF_LIB_THREAD @executable_path/../lib/libfftw3f_threads.3.dylib $loop
install_name_tool -change $GFORTRAN_LIB  @executable_path/../lib/libgfortran.3.dylib $loop
install_name_tool -change $QUADMATH_LIB @executable_path/../lib/libquadmath.0.dylib  $loop
install_name_tool -change $CPP_LIB @executable_path/../lib/libstdc++.6.dylib  $loop
install_name_tool -change $GCC_LIB @executable_path/../lib/libgcc_s.1.dylib  $loop
install_name_tool -change $OMP_LIB @executable_path/../lib/libgomp.1.dylib  $loop
install_name_tool -change $SYS_LIB @executable_path/../lib/libSystem.B.dylib  $loop
otool -L $loop
done

#apps="2dx_image/2dx_image.app/Contents/PlugIns/imageformats 2dx_merge/2dx_merge.app/Contents/PlugIns/imageformats"
#for loop in $apps
#do
#        path=$build_dir"/"$loop	
#	echo "in $path"
#	for loop in `ls $path`
#	do
#		file=$path"/"$loop
#		echo "changing the dylibs of" $loop
#		echo "with absolute path" $file
#		install_name_tool -change QtGui.framework/Versions/4/QtGui @executable_path/../Frameworks/QtGui.framework/Versions/4/QtGui $file 	
#		install_name_tool -change QtCore.framework/Versions/4/QtCore @executable_path/../Frameworks/QtCore.framework/Versions/4/QtCore $file 
#		install_name_tool -change $CPP_LIB @executable_path/../MacOS/libstdc++.6.dylib $file  
#		install_name_tool -change $GCC_LIB @executable_path/../MacOS/libgcc_s.1.dylib $file  
#		otool -L $file
#		if [ $loop = "libqsvg.dylib" ] ; then
#			echo "removing $file"
#			rm $file
#		fi
#	done
#done


#executables="2dx_image/2dx_image.app/Contents/MacOS/2dx_image 2dx_merge/2dx_merge.app/Contents/MacOS/2dx_merge"
#for exe in $executables 
#do
#	file="$build_dir/$exe"
#	echo "changing the dylibs of $file"
#	install_name_tool -change $FFTW_LIB @executable_path/libfftw3f.3.dylib $file
#	install_name_tool -change $FFTW_LIB_THREAD @executable_path/libfftw3f_threads.3.dylib $file
#	install_name_tool -change $QUADMATH_LIB @executable_path/libquadmath.0.dylib  $file 
#	install_name_tool -change $GFORTRAN_LIB  @executable_path/libgfortran.3.dylib $file 
#	install_name_tool -change $QUADMATH_LIB  @executable_path/libquadmath.0.dylib $gfortran.3.dylib
#	otool -L $file 
#done
#
#lib_path="$build_dir/2dx_image/2dx_image.app/Contents/MacOS"
#echo "install_name_tool -change $QUADMATH_LIB  $target_lib_path/libquadmath.0.dylib $lib_path/libgfortran.3.dylib"
#install_name_tool -change $QUADMATH_LIB  $target_lib_path/libquadmath.0.dylib $lib_path/libgfortran.3.dylib
#
#lib_path="$build_dir/2dx_merge/2dx_merge.app/Contents/MacOS"
#echo "install_name_tool -change $QUADMATH_LIB  $target_lib_path/libquadmath.0.dylib $lib_path/libgfortran.3.dylib"
#install_name_tool -change $QUADMATH_LIB  $target_lib_path/libquadmath.0.dylib $lib_path/libgfortran.3.dylib
