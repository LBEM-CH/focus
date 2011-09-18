#!/bin/csh -x
setenv NCPUS 8
#
#   ctffind3
#
time ./ctffind3_mp.exe << eof
micrograph.mrc
montage.pow
2.0,200.0,0.07,60000,7.0		!CS[mm],HT[kV],AmpCnst,XMAG,DStep[um]
128,200.0,8.0,5000.0,30000.0,1000.0	!Box,ResMin[A],ResMax[A],dFMin[A],dFMax[A],FStep
eof
#

