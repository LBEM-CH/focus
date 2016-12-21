#!/bin/sh
# change_dylibs
#
# scirpt to change the dylibs of the Focus and MRC binaries
#
# Marcel Arheit
# 
# Adapted by Sebastian Scherer, Nikhil Biyani
#
#
# How to add antoher library 
# 1. Add if statement 
# 2. Copy Lib to install_dir/loop
# 3. install_name_tool for Lib

if [ $# -lt 1 ]
then
    echo "No install directory specified"
    echo "Usage: `basename $0:` <install_dir>" >&2
    echo 'Aborting!'
    exit 1
fi

install_dir=$1
echo "the install dir is $1"
if [ -f /usr/local/lib/libfftw3.3.dylib ]; then
    FFTW_LIB=/usr/local/lib/libfftw3.3.dylib
    echo "Found FFTW in $FFTW_LIB"
else
    FFTW_LIB=NOT_FOUND
    echo "FFTW not FOUND!"
    exit 2
fi

if [ -f /usr/local/lib/libfftw3f.3.dylib ]; then
    FFTWF_LIB=/usr/local/lib/libfftw3f.3.dylib
    echo "Found FFTWF in $FFTWF_LIB"
else
    FFTWF_LIB=NOT_FOUND
    echo "FFTWF not FOUND!"
    exit 2
fi

if [ -f /usr/local/lib/libfftw3_threads.3.dylib ]; then
    FFTW_LIB_THREAD=/usr/local/lib/libfftw3_threads.3.dylib
    echo "Found FFTW-threaded in $FFTW_LIB_THREAD"
else
    FFTW_LIB_THREAD=NOT_FOUND
    echo "FFTW-threaded not FOUND!"
    exit 2
fi

if [ -f /usr/local/lib/libfftw3f_threads.3.dylib ]; then
    FFTWF_LIB_THREAD=/usr/local/lib/libfftw3f_threads.3.dylib
    echo "Found FFTWF-threaded in $FFTWF_LIB_THREAD"
else
    FFTWF_LIB_THREAD=NOT_FOUND
    echo "FFTWF-threaded not FOUND!"
    exit 2
fi

# BUILD SYSTEM DEPENDENT, FIX IT LATER
if [ -f /usr/local/lib/libgfortran.3.dylib ]; then
    GFORTRAN_LIB=/usr/local/lib/libgfortran.3.dylib
    echo "Found gfortran in $GFORTRAN_LIB"
else
    GFORTRAN_LIB=NOT_FOUND
    echo "gfortran not FOUND!"
    exit 2
fi

if [ -f /usr/local/lib/libquadmath.0.dylib  ]; then
    QUADMATH_LIB=/usr/local/lib/libquadmath.0.dylib
    echo "Found quadmath in $QUADMATH_LIB"
else
    QUADMATH_LIB=NOT_FOUND
    echo "quadmath not FOUND!"
    exit 2
fi

if [ -f /usr/local/lib/libstdc++.6.dylib ]; then
    CPP_LIB=/usr/local/lib/libstdc++.6.dylib
    echo "Found libstdc++.6.dylib in $CPP_LIB"
else
    CPP_LIB=NOT_FOUND
    echo "libstdc++ not FOUND!"
    exit 2
fi

if [ -f /usr/local/lib/libgcc_s.1.dylib ]; then
    GCC_LIB=/usr/local/lib/libgcc_s.1.dylib
    echo "Found libgcc_s.1.dylib in $GCC_LIB"
else
    GCC_LIB=NOT_FOUND
    echo "libgcc_s.1.dylib not FOUND!"
    exit 2
fi

if [ -f /usr/local/lib/libgomp.1.dylib ]; then
    OMP_LIB=/usr/local/lib/libgomp.1.dylib
    echo "Found libgomp.1.dylib in $OMP_LIB"
else
    OMP_LIB=NOT_FOUND
    echo "libgomp.1.dylib not FOUND!"
    exit 2
fi

# sys-lib
if [ -f  /usr/lib/libSystem.B.dylib ]; then
    SYS_LIB=/usr/lib/libSystem.B.dylib
    echo "Found libSystem.B.dylib in $SYS_LIB"
fi

binaries="kernel/mrc/lib"
for loop in $binaries
do
    echo "cp $FFTW_LIB  $install_dir/$loop"
    cp $FFTW_LIB $install_dir/$loop
    echo "cp $FFTWF_LIB  $install_dir/$loop"
    cp $FFTWF_LIB $install_dir/$loop
    echo "cp $FFTW_LIB_THREAD  $install_dir/$loop"
    cp $FFTW_LIB_THREAD $install_dir/$loop
    echo "cp $FFTWF_LIB_THREAD  $install_dir/$loop"
    cp $FFTWF_LIB_THREAD $install_dir/$loop	
    echo "cp $GFORTRAN_LIB $install_dir/$loop"
    cp $GFORTRAN_LIB $install_dir/$loop
    echo "cp $QUADMATH_LIB $install_dir/$loop"
    cp $QUADMATH_LIB $install_dir/$loop
    echo "cp $CPP_LIB $install_dir/$loop"
    cp $CPP_LIB $install_dir/$loop
    echo "cp $GCC_LIB $install_dir/$loop"
    cp $GCC_LIB $install_dir/$loop
    echo "cp $OMP_LIB $install_dir/$loop"
    cp $OMP_LIB $install_dir/$loop
    echo "cp $SYS_LIB $install_dir/$loop"
    cp $SYS_LIB $install_dir/$loop
done

fortran_bin="kernel/mrc/bin"
path="$install_dir/$fortran_bin"
lib_path="$install_dir/kernel/mrc/lib"

#echo "install_name_tool -change $QUADMATH_LIB  $target_lib_path/libquadmath.0.dylib $lib_path/libgfortran.3.dylib"
#install_name_tool -change $QUADMATH_LIB  $target_lib_path/libquadmath.0.dylib $lib_path/libgfortran.3.dylib

for libfile in `ls -1 $lib_path`
do
    chmod u+w $lib_path/$libfile
done

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

for loop in $install_dir/$binaries/*.dylib
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
