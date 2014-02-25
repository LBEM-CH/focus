#!/bin/bash
#
# scirpt to package the 2dx installer with package maker 
#
# Marcel Arheit  21.01.2011

if [ $# -lt 2 ]
then
	echo "Not enough arguments specified"
	echo "Usage: `basename $0:` <root_dir> <out_file>" >&2
	echo 'Aborting!'
	exit 1
fi
DIR=`dirname $0`
echo "DIR=$DIR"
ROOT=$1
OUTFILE=$2
if [ -f $OUTFILE ]
then
	echo "removing previous version of $OUTFILE" 
	rm $OUTFILE
fi

echo "rec-dir: $ROOT/Resource"
ls $ROOT/Resource


packagemaker \
--title "2dx Nightly Build" \
--version  DATE=`date "+%d_%b_%Y"` \
--filter "\.DS_Store" \
--resources "$DIR/Resources" \
--scripts $DIR/Resources/scripts \
--root-volume-only \
--domain system \
--verbose \
--no-relocate \
-l "/opt/2dx" \
--target 10.8 \
--id org.2dx.pkg \
--root $ROOT \
--out $OUTFILE \
--discard-forks \
--verbose
