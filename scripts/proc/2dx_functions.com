#!/bin/sh

USE_COLORS()
{
  ESC="\033"
  D="0"
  B="1"

  NORMAL="\033[m"
  BLUE='\033[1;34m'
  PURPLE='\033[1;35m'
  RED='\033[1;31m'
}

checkFile()
{
  if [ ! -e $1 ]; then
    ${proc_2dx}/linblock "#"
    ${proc_2dx}/linhash "ERROR: $1 not found."
    ${proc_2dx}/linhash "ERROR: You might first need to run previous scripts."
    ${proc_2dx}/linblock "#"
  fi
}

help()
{
  USE_COLORS

  echo ""
  echo -e $BLUE"2dx_functions.com"$NORMAL":"
  echo ""
  echo "This is a simple bash script designed to be wrapped in other contexts."
  echo "Functions are useful and are easily created in bash:"
  echo ""
  echo -e "${RED}testFunction${NORMAL}()"
  echo "{"
  echo -e ${PURPLE}'  echo '$RED'"Hello this is a test"'$NORMAL
  echo "}"
  echo ""
  echo "Functions defined here can then be called via:"
  echo ""
  echo -e ${BLUE}"2dx_functions.com"${NORMAL}' ${functionName} ${functionArguments}'
  echo ""
  echo "This flexibly allows programmatic function calls based on strings"
  echo "and parameters, making workflow decisions easier. i.e.:"
  echo ""
  echo -e ${BLUE}"2dx_functions.com"${NORMAL}' ${origtiltversion} ${argumentsCommonToBothVersions}'
  echo ""
  echo "2dx_functions.com also provides the full range of typical bash functions,"
  echo "when called."
  echo ""
  echo 'For instance ${origtiltversion} might be drawn from a list of options'
  echo 'including the system command "echo" allowing full bash integration.'
  echo -e 'Try "'"${BLUE}2dx_functions.com${NORMAL} ls"'" for instance'
  echo ""
  
}

parse()
{
  echo $@ | awk 'BEGIN{ FS="|" } { split($1,v," "); if(v[1]==1) print v[3]; else {s=$(v[1]);gsub(/(^\ +)|(\ +$)/,"",s);  print s; } }'
}

origtiltk()
{      
  local bin_2dx=`parse 1 $@`
  local spcgrp=`parse 2 $@`
  local realcell=`parse 3 $@`
  local ALAT=`parse 4 $@`
  local realang=`parse 5 $@`
  local IAQP2=`parse 6 $@`
  local IVERBOSE=`parse 7 $@`
  local LOGOUTPUT=`parse 8 $@`
  local imagenumber=`parse 9 $@`
  local phastepnum=`parse 10 $@`
  local RFACAMP=`parse 11 $@`
  local aphdummy=`parse 12 $@`
  local imagename=`parse 13 $@`
  local date=`parse 14 $@`
  local aphfile=`parse 15 $@`
  local TAXA=`parse 16 $@`
  local TANGL=`parse 17 $@`
  local lattice=`parse 18 $@`
  local phaori_local=`parse 19 $@`
  local phastep=`parse 20 $@`
  local zwin=`parse 21 $@`
  local sgnxchval=`parse 22 $@`
  local SCL=`parse 23 $@`
  local ctfrevval=`parse 24 $@`
  local rot90val=`parse 25 $@`
  local revhndval=`parse 26 $@`
  local revxsgnval=`parse 27 $@`
  local LPROTFOUFIL=`parse 28 $@`
  local CS=`parse 29 $@`
  local KV=`parse 30 $@`
  local beamtilt=`parse 31 $@`
  local RESMIN=`parse 32 $@`
  local RESMAX=`parse 33 $@`
  local rot180val=`parse 34 $@`
  local revhkval=`parse 35 $@`
  

  echo ":Parameter for 2dx_origtiltk.exe are:"
  echo ":================================================================================="
  echo :SCRATCH/2dx_origtilt-LOG1.dat
  echo :${spcgrp},0,F,F,1,${realcell},${ALAT},${realang},0,15,${IAQP2},${IVERBOSE},${LOGOUTPUT}
  echo :10,0.7,10,0.5 
  echo :${imagenumber},0,30,5,${phastepnum},T,F,${RFACAMP} 
  echo :100,DUMMY
  echo :${aphdummy}
  echo :${imagenumber},${imagename},${date}
  echo :${aphfile}
  echo :F
  echo :${TAXA},${TANGL},0 
  echo :${lattice}
  echo :${phaori_local},${phastep},${zwin},${sgnxchval},${SCL},${rot180val},${revhkval},${ctfrevval},${rot90val},${revhndval},${revxsgnval},${LPROTFOUFIL}
  echo :${CS},${KV},${beamtilt}                                                
  echo :${RESMIN},${RESMAX}                                                
  echo :-1
  echo ":================================================================================="
  echo " "
  #
  \rm -f 2dx_origtiltk-console.log
  #

  ${bin_2dx}/2dx_origtiltk.exe << eot
SCRATCH/2dx_origtilt-LOG1.dat
SCRATCH/TMP.tmp.reflections
SCRATCH/TMP.tmp.console
${spcgrp},0,F,F,1,${realcell},${ALAT},${realang},0,15,${IAQP2},${IVERBOSE},${LOGOUTPUT} !ISPG,NPRG,NTL,NBM,ILST,A,B,W,ANG,IPL,MNRF,IAQP2,IVERBOSE,LOGOUTPUT,LPROTFOUFIL
10,0.7,10,0.5                                                   ! itaxastep,rtaxasize,itanglstep,rtanglsize
${imagenumber},0,44,6,${phastepnum},F,F,${RFACAMP}          	!IRUN,LHMN,LHMX,IQMX,IBXPHS,NREFOUT,NSHFTIN,RFACAMP
100,DUMMY
${aphdummy}
${imagenumber},${imagename},${date}
${aphfile}
F
${TAXA},${TANGL},0                                              ! TAXA,TANGL,IORIGT
${lattice} 							! Reciprocal lattice
${phaori_local},${phastep},${zwin},${sgnxchval},${SCL},${rot180val},${revhkval},${ctfrevval},${rot90val},${revhndval},${revxsgnval},${LPROTFOUFIL} ! OH,OK,STEP,WIN,SGNXCH,SCL,ROT,REV,CTFREV,ROT90,REVHND,REVXSGN,LPROTFOUFIL
${CS},${KV},${beamtilt}                                         ! cs,kv,tx,ty
${RESMIN},${RESMAX}                                             ! resolution limits
-1
eot

  cat 2dx_origtiltk-console.log
  \rm -f SCRATCH/TMP.tmp.reflections
  \rm -f SCRATCH/TMP.tmp.console

}

if [ $# -eq 0 ]; then
  help
elif [ $# -eq 1 ]; then
  $1
else
  $1 $@
fi

