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
    otool -L $file > tmp.otool
    tail -n +2 tmp.otool > tmp.paths
    while read p; do
        deplibpath=`echo $p | cut -d'(' -f1 | tr -d " \t\n\r"`
        if [[ $deplibpath != @* ]]; then
            deplibdir=`dirname $deplibpath`
            deplib=`basename $deplibpath`
            if [ ! -d $lib_path/$deplibdir ]; then
                mkdir -p $lib_path/$deplibdir
            fi
            if [ ! -f $lib_path/$deplibdir/$deplib ]; then
                echo "Copying: $deplibpath $lib_path/$deplibdir/$deplib"
                cp $deplibpath $lib_path/$deplibdir/$deplib
                chmod u+w $lib_path/$deplibdir/$deplib
            fi
            install_name_tool -change $deplibpath @executable_path/../lib/$deplibdir/$deplib $file
        fi
    done <tmp.paths
    rm tmp.paths tmp.otool
    echo "New paths:"
    otool -L $file 
done

for libname in `find $lib_path -name "*.dylib"`
do
    file=$libname
    echo "========================================================="
    echo "changing the dylibs of $file"

    # Gather all the dependent libraries
    otool -L $file > tmp.otool
    tail -n +2 tmp.otool > tmp.paths
    while read p; do
        deplibpath=`echo $p | cut -d'(' -f1 | tr -d " \t\n\r"`
        deplibdir=`dirname $deplibpath`
        deplib=`basename $deplibpath`
        if [[ $deplibpath != @* ]]; then
            if [[ $deplibpath != /usr/lib/system* ]]; then
                deplibdir=`dirname $deplibpath`
                deplib=`basename $deplibpath`
                if [ ! -d $lib_path/$deplibdir ]; then
                    mkdir -p $lib_path/$deplibdir
                fi
                if [ ! -f $lib_path/$deplibdir/$deplib ]; then
                    echo "Copying: $deplibpath $lib_path/$deplibdir/$deplib"
                    cp $deplibpath $lib_path/$deplibdir/$deplib
                    chmod u+w $lib_path/$deplibdir/$deplib
                fi
                install_name_tool -change $deplibpath @executable_path/../lib/$deplibdir/$deplib $file
            fi
        fi
    done <tmp.paths
    rm tmp.paths tmp.otool
    echo "New paths:"
    otool -L $file
done
