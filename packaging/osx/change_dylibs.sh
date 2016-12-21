#!/bin/sh
# change_dylibs
#
# scirpt to change the dylibs of the Focus and MRC binaries
#
# Author: Nikhil Biyani

if [ $# -lt 1 ]
then
    echo "No install directory specified"
    echo "Usage: `basename $0:` <install_dir>" >&2
    echo 'Aborting!'
    exit 1
fi

install_dir=$1
echo "the install dir is $1"

path="$install_dir/kernel/mrc/bin"
lib_path="$install_dir/kernel/mrc/lib"

echo "Changing binaries in $path" 
for exe in `ls -1 $path`
do
    file="$path/$exe"
    echo "========================================================="
    echo "changing the dylibs of $file"
    
    # Gather all the dependent libraries
    otool -LX $file > tmp.paths
    while read p; do
        deplibpath=`echo $p | cut -d'(' -f1 | tr -d " \t\n\r"`
        deplib=`basename $deplibpath`
        if [ ! -f $lib_path/$deplib ]; then
            echo "Copying: $deplibpath $lib_path/$deplib"
            cp $deplibpath $lib_path/$deplib
            chmod u+w $lib_path/$deplib
        fi
        install_name_tool -change $deplibpath @executable_path/../lib/$deplib $file
    done <tmp.paths
    rm tmp.paths
    echo "New paths:"
    otool -L $file 
done

for libname in `ls -1 $lib_path/*.dylib`
do
    file=$libname
    echo "========================================================="
    echo "changing the dylibs of $file"

    # Gather all the dependent libraries
    otool -LX $file > tmp.paths
    while read p; do
        deplibpath=`echo $p | cut -d'(' -f1 | tr -d " \t\n\r"`
        deplib=`basename $deplibpath`
        if [ -f $lib_path/$deplib ]; then
        	install_name_tool -change $deplibpath @executable_path/../lib/$deplib $file
        fi
    done <tmp.paths
    rm tmp.paths
    echo "New paths:"
    otool -L $file
done
