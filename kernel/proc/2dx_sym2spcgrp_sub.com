#
#  2dx_sym2spcgrp-sub.com
#
#################################################################################
#################################################################################
#################################################################################
#
# This file tries to assign the CCP4 space group number to the chosen MRC 
# space group number. This might work for a few space groups, but there are 
# probably many other space groups where the settings are plain wrong.
# Any help in debugging this is greatly appreciated !!! Please contact us at 
# 2dx.org, if you have debugged one of the many space groups that were not working
# before.
#
#################################################################################
#################################################################################
#################################################################################
#
#
# Attention:
# This is not an independent csh file.
# This file has to be sourced from another csh.
#
# Input is in ${SYM}
# Output is in ${spcgrp} for the normal MRC spacegroup numbers
# Output is in ${spcgroup_num} for the numbers used in ALLSPACE.
# Output is in ${CCP4_SYM} for the CCP4 annotation of the space group.
# Output is in ${docentric}, which tells if "centric" needs to be applied. (discontinued)
# Output is in ${ML_sym_val} for the rotational symmetry.
# Output is in ${rotate_to_Z} to indicate if reassignment of axes is needed (for p2, p2221, etc.)
#
#
#################################################################################
# MRC Space Group Naming convention:
#################################################################################
#
# The MRC programs including ALLSPACE recognize the following space groups:
# p1;p2;p12_a;p12_b;p121_a;p121_b;c12_a;c12_b;p222;p2221a;p2221b;p22121;c222;p3;p312;p4;p422;p4212;p6;p622
#
# These are differently oriented versions of the 17 planar space groups as below:
#
# This is the information that can be found in ORIGTILT:
#
#     NUMBER   SPACEGROUP     ASYMMETRI# UNIT        REAL      IMAGINARY
#
#          1          P1         H>=0
#
#          2         P21         H,Z>=0              Z=0
#
#          3         P12         H,K>=0              K=0
#
#          4        P121         H,K>=0              K=0
#
#          5         C12         H,K>=0              K=0
#
#          6        P222         H,K,Z>=0            H=0
#                                                    K=0
#                                                    Z=0
#
#          7       P2221         H,K,Z>=0            (0,2N,Z)  (0,2N+1,Z)
#                                                    (H,K,0)
#                                                    (H,0,Z)
#                                                    H=K=even  H=K=odd
#
#          8      P22121         H,K,Z>=0            (H,K,0)
#                                                    (2N,0,Z)  (2N+1,0,Z)
#                                                    (0,2N,Z)  (0,2N+1,Z)
#
#          9        C222         H,K,Z>=0            (H,K,0)
#                                                    (H,0,Z)
#                                                    (0,K,Z)
#
#         10          P4         H,K,Z>=0            (H,K,0)
#
#         11        P422         H,K,Z>=0            (H,K,0)
#                                K>=H                (H,0,Z)
#                                                    (0,K,Z)
#                                                    (H,H,Z)
#
#         12       P4212         H,K,Z>=0            (H,K,0)
#                                K>=H                (H,H,Z)
#                                                    (2N,0,Z)   (2N+1,0,Z)
#                                                    (0,2N,Z)   (0,2N+1,Z)
#
#         13          P3         H,K>=0
#
#         14        P312         H,K>=0              (H,H,Z)
#                                K>=H
#
#         15        P321         H,K>=0              (H,0,Z)
#                                K>H                 (0,K,Z)
#
#         16          P6         H,K,Z>=0            (H,K,0)
#
#         17        P622         H,K,Z>=0            (H,K,0)
#                                K>=H                (H,H,Z)
#
#******************************************************************************
#******************************************************************************
#******************************************************************************
#
#  CCP4 Space group numbers:
#
#        1:  P1          2:  P-1         3:  P2            4:  P21
#        5:  C2         10:  P2/m       16:  P222         17:  P2221
#       18: P21212    1018: P21212      19: P212121       20:C2221
#       21:  C222       22:  F222       23:  I222         24: I212121
#       47:  Pmmm       65:  Cmmm       69:  Fmmm         71:  Immm
#       75:  P4         76:  P41        77:  P42          78:  P43
#       79:  I4         80:  I41        83:  P4/m         87:  I4/m
#       89: P422        90: P4212       91: P4122         92: P41212
#       93: P4222       94: P42212      95: P4322         96: P43212
#       97: I422        98: I4122      123: P4/mmm       139: I4/mmm
#      143:  P3        144:  P31       145: P32          146:  R3
#      147:  P-3       148:  R-3       149: P312         150:  P321
#      151: P3112      152: P3121      153: P3212        154: P3221
#      155: R32        162:  P-31m     164: P-3m1
#      166:  R-3m      168:  P6
#      169:  P61       170:  P65       171:  P62         172:  P64
#      173:  P63       175:  P6/m      177: P622         178: P6122
#      179: P6522      180: P6222      181: P6422        182: P6322
#      191: P6/mmm     195: P23        196: F23          197: I23
#      198: P213       199: I213       200: Pm-3         202: Fm-3
#      204: Im-3       207: P432       208: P4232        209: F432
#      210: F4132      211: I432       212: P4332        213: P4132
#      214: I4132      221: Pm-3m      225: Fm-3m        229: Im-3m
#
# There is also this non-standard definition:
#    1 P1               3 P2              -3 P112             4 P21       
#   -4 P1121            5 C2              -5 A112            16 P222      
#   17 P2221           18 P21212          19 P212121         20 C2221     
#   21 C222            22 F222            23 I222            24 I212121   
#   75 P4              76 P41             77 P42             78 P43       
#   79 I4              80 I41             89 P422            90 P4212     
#   91 P4122           92 P41212          93 P4222           94 P42212    
#   95 P4322           96 P43212          97 I422            98 I4122     
#  143 P3             144 P31            145 P32            146 R3        
# -146 R3R            149 P312           150 P321           151 P3112     
#  152 P3121          153 P3212          154 P3221          155 R32       
# -155 R32R           168 P6             169 P61            170 P65       
#  171 P62            172 P64            173 P63            177 P622      
#  178 P6122          179 P6522          180 P6222          181 P6422     
#  182 P6322          195 P23            196 F23            197 I23       
#  198 P213           199 I213           207 P432           208 P4232     
#  209 F432           210 F4132          211 I432           212 P4332     
#  213 P4132          214 I4132         1003 P112
#
# This information is from Per Bullough, University Sheffield, UK (Thanks a lot, Per!!!):
# The set above has negative space group numbers for special orientations
# of certain space groups. 
# For example: P2 (CCP4=3) in CCP4 has the dyad along Y. 
# If you want to use the dyad along Z (as used in MRC), one should use P112, 
# which is (CCP4=1003) or (CCP4=-3).
#
#******************************************************************************
#******************************************************************************
#******************************************************************************
#
# This is the information that can be found in ALLSPACE:
#
#   Table of phase comparisons to be made
#       -  not comparable       
#       1  directly identical
#       H  differ by 180 * H            JSIMPL  = number to compare directly
#       K  differ by 180 * K            JSCREW   = number to compare + 180 * M
#       HK differ by 180 * (H+K)         where M = H*JH180 + K*JK180
#                                        
#
#   SPACEGROUP  H=-: -h +k +k -k -k +h -h +k -k -h +h -h +h  JSIMPL
#               H=                                 -k +k -k +k     JSCREW
# ref in
#  prog # symb  K=+k -k -k +h -h +h -h -h +h -h +h +h -h +k -k         JH180
#               K=                     -k +k -k +k                         JK180
#
#  1    1   p1     -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   0  0   -   -
#  2    2   p2     -  -  1  -  -  -  -  -  -  -  -  -  -  -  -   1  0   -   -
#  3    3b  p12    1  -  -  -  -  -  -  -  -  -  -  -  -  -  -   1  0   -   -
#  4    "a   "     -  1  -  -  -  -  -  -  -  -  -  -  -  -  -   1  0   -   -
#  5    4b  p121   K  -  -  -  -  -  -  -  -  -  -  -  -  -  -   0  1   -  180
#  6    "a   "     -  H  -  -  -  -  -  -  -  -  -  -  -  -  -   0  1  180  -
#  7    5b  c12    1  -  -  -  -  -  -  -  -  -  -  -  -  -  -   1  0   -   -
#  8    "a   "     -  1  -  -  -  -  -  -  -  -  -  -  -  -  -   1  0   -   -
#  9    6   p222   1  1  1  -  -  -  -  -  -  -  -  -  -  -  -   3  0   -   -
# 10    7b  p2221  H  H  1  -  -  -  -  -  -  -  -  -  -  -  -   1  2  180  -
# 11    "a    "    K  K  1  -  -  -  -  -  -  -  -  -  -  -  -   1  2   -  180
# 12    8   p22121 HK HK 1  -  -  -  -  -  -  -  -  -  -  -  -   1  2  180 180
# 13    9   c222   1  1  1  -  -  -  -  -  -  -  -  -  -  -  -   3  0   -   -
# 14    10  p4     -  -  1  -  1  1  -  -  -  -  -  -  -  -  -   3  0   -   -
# 15    11  p422   1  1  1  1  1  1  1  -  -  -  -  -  -  -  -   7  0   -   -
# 16    12  p4212  HK HK 1  1  HK HK 1  -  -  -  -  -  -  -  -   3  4  180 180
# 17    13  p3     -  -  -  -  -  -  -  -  -  1  -  1  -  -  -   2  0   -   -
# 18    14  p312   -  -  -  -  -  -  1  -  1  1  -  1  -  -  1   5  0   -   -
# 19    15  p321   -  -  -  1  -  -  -  1  -  1  -  1  -  1  -   5  0   -   -
# 20    16  p6     -  -  1  -  -  -  -  -  -  1  1  1  1  -  -   5  0   -   -
# 21    17  p622   -  -  1  1  -  -  1  1  1  1  1  1  1  1  1   11 0   -   -
#
#******************************************************************************
#******************************************************************************
#******************************************************************************
#
# Per Bullough from the University of Sheffield, UK, answered our questions and 
# came to our help with the following (April 2007):
#
# p2: The standard sg number for p2 in CCP4 is 3 but this has the dyad along Y. 
# Instead you should use number 1003 which has the dyad along Z.
#
# p12_a and p12_b both correspond to p2 with the dyad along X and along Y respectively.
# In CCP4 there is no equivalent of p12_a and in this case you will have to reassign axes. 
# p12_b is equivalent to the CCP4 standard setting for P2, space group number 3. 
# The only REAL amplitudes are along k=0 so CENTRIC should only convert phases to 0 or 180 for k=0.
# You will need to check which convention ORIGTILTD uses but I suspect it is with the dyad along Y.
#
# p121_a and p121_b correspond to 2fold screws (21) along X and Y respectively.  
# The standard CCP4 setting has the screw along Y and is space group number 4. 
# There is no CCP4 equivalent to p121_a. I suspect ORIGTILTD uses the standard setting, 
# but you would need to check. Note also that in CCP4 you can explicitly provide symmetry 
# operators if you want to use non-standard settings, but this is messy- it might be 
# neater just to reassign axes.
# For p121_b CENTRIC should only convert to 0 or 180 for k=0. Note that as it stands, 
# CENTRIC currently converts for all (h.k) values, so you would need to write some new code. 
# In the past in cases such as this we have simply converted by hand since there are so few reflections.
#
# c12_a and c12_b are equivalent to C2 in CCP4. 
# The standard setting has dyad along Y giving space group number 5 in CCP4. 
# There is no equivalent to c12_a so again it is best to swap axes. 
# Again CENTRIC should only apply to k=0.
#
# for p222 CCP4 space group number is 16. CENTRIC should be applied to all reflections.
#
# p2221_a and p2221_b have 2fold screws along X and Y respectively and are equivalent 
# to P2221 in CCP4 - space group number 17. The standard setting has the 2fold screw along Z 
# so either you have to reassign the axes or explicitly give the symmetry operators in CCP4. 
# CENTRIC should be applied to all (h,K).
#
# p22121 in MRC has the twofold down Z. 
# In CCP4 it is called P21212 and has number 18. 
# However, there is also a space group number 1018 which has a 1/4, 1/4, 0 origin shift. 
# I'm not sure what's going on in terms of orgin choice here so I think this space group 
# needs careful testing. (Henning: No, 18 seems correct.) CENTRIC applies to all (h,k) values.
#
# c222 is equivalent to C222 in CCP4 (number 21). CENTRIC applies to all (H,K). 
# Note the requirement for systematic absences.
#
# p3 is equivalent to P3 in CCP4 (number 143). CENTRIC does not apply.
#
# p312 is equivalent to P312 in CCP4 (number 149). CENTRIC applies for h=k only
#
# p321 is equivalent to P321 in CCP4 (number 150). CENTRIC only applies to (h,0) and (0,k).
#
# p4 is equivalent to P4 (number 75) in CCP4. CENTRIC applies to all (h,k).
#
# p422 is equivalent to P422 (number 89) in CCP4. CENTRIC applies to all (h,k).
#
# p4212 is equivalent to P4212 in CCP (number 90). CENTRIC applies to all (h,k). 
# Note systematic absences for (2n+1,0) and (0,2n+1).
#
# p6 is equivalent to CCP4 P6 (168). CENTRIC applies to all (h,k)
#
# p622 is equivalent to CCP4 P622 (177). CENTRIC applies to all (h,k)
#
#
# By the way a VERY IMPORTANT POINT is that space groups with 21 screws or c-centering 
# should have systematic absences e.g. amplitude = 0.0 for and (h,k) = (0, 2n+1) 
# for a 21 axis along Y. This is not taken care of in any of the MRC programs and in 
# the past I have always edited them out by hand. If the image is well behaved these 
# reflections should show up with very high IQ values anyway. 
# However if you get a low IQ for what should be an absent reflection it would be worth 
# flagging this up in your program with a warning.
#
# You should be aware that for all of the space groups CENTRIC only applies to projection data i.e. (h,k,0).
#
#
#******************************************************************************
#******************************************************************************
#******************************************************************************
#****** All the above can be summarized in the following information: *********
#******************************************************************************
#******************************************************************************
#******************************************************************************
#
#   Table of phase comparisons to be made
#       -  not comparable       
#       1  directly identical
#       H  differ by 180 * H            JSIMPL  = number to compare directly
#       K  differ by 180 * K            JSCREW   = number to compare + 180 * M
#       HK differ by 180 * (H+K)         where M = H*JH180 + K*JK180
#       C  CENTRIC needed (phase to 0 or 180 for h,0 or 0,k)
#       SX CENTRIC special treatment required, applied only along X axis (k=0)
#       SD CENTRIC special treatment required, applied only along diagonal (h=k)
#       SP CENTRIC special treatment required, applied only along Plus (h,0) and (0,k)
#                                        
#   SPACEGROUP  H=-h +h -h +k +k -k -k +h -h +k -k -h +h -h +h  JSIMPL
#               H=                                 -k +k -k +k     JSCREW   
# In MRC progs                                                               CENTRIC
# ALLSPACE/ORIGTILT                                                                 In CCP4
# numb  # symb  K=+k -k -k +h -h +h -h -h +h -h +h +h -h +k -k         JH180       numb symb    comment
#               K=                     -k +k -k +k                         JK180
#
#  1    1   p1     -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   0  0   -   -   -    1   P1
#  2    2   p2     -  -  1  -  -  -  -  -  -  -  -  -  -  -  -   1  0   -   -   C    3   P2     but dyad along Y. Use CCP4=1003 or reassign axis.
#  3    3b  p12    1  -  -  -  -  -  -  -  -  -  -  -  -  -  -   1  0   -   -   SX   3   P2
#  4    "a   "     -  1  -  -  -  -  -  -  -  -  -  -  -  -  -   1  0   -   -   -    ------     reassign axis, and use CCP4=3 above.
#  5    4b  p121   K  -  -  -  -  -  -  -  -  -  -  -  -  -  -   0  1   -  180  SX   4  P21
#  6    "a   "     -  H  -  -  -  -  -  -  -  -  -  -  -  -  -   0  1  180  -   -    ------     reassign axis, and use CCP4=4 above.
#  7    5b  c12    1  -  -  -  -  -  -  -  -  -  -  -  -  -  -   1  0   -   -   SX   5   C2
#  8    "a   "     -  1  -  -  -  -  -  -  -  -  -  -  -  -  -   1  0   -   -   -    ------     reassign axis, and use CCP4=5 above.
#  9    6   p222   1  1  1  -  -  -  -  -  -  -  -  -  -  -  -   3  0   -   -   C    16  P222 
# 10    7b  p2221  H  H  1  -  -  -  -  -  -  -  -  -  -  -  -   1  2  180  -   -    17  P2221  CCP4 P2221 has 2fold screw in Z. Reassign axis.
# 11    "a    "    K  K  1  -  -  -  -  -  -  -  -  -  -  -  -   1  2   -  180  -    ------     reassign axis, and use CCP4=17 above.
# 12    8   p22121 HK HK 1  -  -  -  -  -  -  -  -  -  -  -  -   1  2  180 180  C    18  P21212 
# 13    9   c222   1  1  1  -  -  -  -  -  -  -  -  -  -  -  -   3  0   -   -   C    21  C222   Note: Systematic absences
# 14    10  p4     -  -  1  -  1  1  -  -  -  -  -  -  -  -  -   3  0   -   -   C    75  P4
# 15    11  p422   1  1  1  1  1  1  1  -  -  -  -  -  -  -  -   7  0   -   -   C    89  P422 
# 16    12  p4212  HK HK 1  1  HK HK 1  -  -  -  -  -  -  -  -   3  4  180 180  -    90  P4212  Note: Systematic absences for (2n+1,0) and (0,2n+1). 
# 17    13  p3     -  -  -  -  -  -  -  -  -  1  -  1  -  -  -   2  0   -   -   -    143 P3
# 18    14  p312   -  -  -  -  -  -  1  -  1  1  -  1  -  -  1   5  0   -   -   SD   149 P312
# 19    15  p321   -  -  -  1  -  -  -  1  -  1  -  1  -  1  -   5  0   -   -   SP   150 P321
# 20    16  p6     -  -  1  -  -  -  -  -  -  1  1  1  1  -  -   5  0   -   -   C    168 P6
# 21    17  p622   -  -  1  1  -  -  1  1  1  1  1  1  1  1  1   11 0   -   -   C    177 P622
#
#******************************************************************************
#******************************************************************************
#******************************************************************************
#
# And now comes an attempt to put that all into a standard translation table between 
# MRC and CCP4...:
#
set verified = "no"
set set_to_90 = "no"
set set_to_120 = "no"
set TWOFOLD = "F"
#
if ( ${SYM} == 'p1' ) then
  set spcgrp = "1"
  set spcgroup_num = "1"
  set CCP4_SYM = "1"
  set ML_sym_val = "1"
  set rotate_to_Z = "no"
  set verified = "yes"

else if ( ${SYM} == 'p2' ) then
  set spcgrp = "2"
  set spcgroup_num = "2"
  set CCP4_SYM = "3"
  set ML_sym_val = "2"
  set rotate_to_Z = "yes"
  set verified = "yes"

else if ( ${SYM} == 'p12_a' ) then
  set spcgrp = "3"
  set spcgroup_num = "4"
  set CCP4_SYM = "3"
  set ML_sym_val = "1"
  set rotate_to_Z = "no"
  echo "::WARNING from proc/2dx_sym2spcgrp-sub.com: Using CCP4_SYM = ${CCP4_SYM}." 
  ${proc_2dx}/linblock "#" 
  ${proc_2dx}/linblock "WARNING symmetry p12_a needs reassignment of axis for CCP4, not yet implemented." 
  ${proc_2dx}/linblock "Use p12_b instead, and rotate the lattice by 90 degrees." 
  ${proc_2dx}/linblock "#" 

else if ( ${SYM} == 'p12_b' ) then
  set spcgrp = "3"
  set spcgroup_num = "3"
  set CCP4_SYM = "3"
  set ML_sym_val = "1"
  set rotate_to_Z = "no"
  echo "::WARNING from proc/2dx_sym2spcgrp-sub.com: Using CCP4_SYM = ${CCP4_SYM}." 

else if ( ${SYM} == 'p121_a' ) then
  set spcgrp = "4"
  set spcgroup_num = "6"
  set CCP4_SYM = "4"
  set ML_sym_val = "1"
  set rotate_to_Z = "no"
  echo "::WARNING from proc/2dx_sym2spcgrp-sub.com: Using CCP4_SYM = ${CCP4_SYM}." 
  ${proc_2dx}/linblock "#" 
  ${proc_2dx}/linblock "WARNING: symmetry p121_a needs reassignment of axis for CCP4, not yet implemented." 
  ${proc_2dx}/linblock "Use p121_b instead, and rotate the lattice by 90 degrees." 
  ${proc_2dx}/linblock "#" 

else if ( ${SYM} == 'p121_b' ) then
  set spcgrp = "4"
  set spcgroup_num = "5"
  set CCP4_SYM = "4"
  set ML_sym_val = "1"
  set rotate_to_Z = "no"
  set verified = "yes"

else if ( ${SYM} == 'c12_a' ) then
  set spcgrp = "5"
  set spcgroup_num = "8"
  set CCP4_SYM = "5"
  set ML_sym_val = "1"
  set rotate_to_Z = "no"
  echo "::WARNING from proc/2dx_sym2spcgrp-sub.com: Using CCP4_SYM = ${CCP4_SYM}." 
  ${proc_2dx}/linblock "#" 
  ${proc_2dx}/linblock "WARNING: symmetry c12_a needs reassignment of axis for CCP4, not yet implemented." 
  ${proc_2dx}/linblock "Use c12_b instead, and rotate the lattice by 90 degrees." 
  ${proc_2dx}/linblock "#" 

else if ( ${SYM} == 'c12_b' ) then
  set spcgrp = "5"
  set spcgroup_num = "7"
  set CCP4_SYM = "5"
  set ML_sym_val = "1"
  set rotate_to_Z = "no"
  echo "::WARNING from proc/2dx_sym2spcgrp-sub.com: Using CCP4_SYM = ${CCP4_SYM}." 

else if ( ${SYM} == 'p222' ) then
  set spcgrp = "6"
  set spcgroup_num = "9"
  set ML_sym_val = "2"
  set CCP4_SYM = "16"
  set rotate_to_Z = "no"

else if ( ${SYM} == 'p2221a' ) then
  set spcgrp = "7"
  set spcgroup_num = "11"
  set CCP4_SYM = "17"
  set ML_sym_val = "2"
  set rotate_to_Z = "yes"
  set TWOFOLD = "T"
  echo "::WARNING from proc/2dx_sym2spcgrp-sub.com: Using CCP4_SYM = ${CCP4_SYM}." 
  ${proc_2dx}/linblock "#" 
  ${proc_2dx}/linblock "WARNING: symmetry p2221_a needs reassignment of axis for CCP4, not yet implemented." 
  ${proc_2dx}/linblock "Use p2221_b instead, and rotate the lattice by 90 degrees." 
  ${proc_2dx}/linblock "#" 

else if ( ${SYM} == 'p2221b' ) then
  set spcgrp = "7"
  set spcgroup_num = "10"
  set CCP4_SYM = "17"
  set ML_sym_val = "2"
  set rotate_to_Z = "yes"
  set TWOFOLD = "T"
  echo "::WARNING from proc/2dx_sym2spcgrp-sub.com: Using CCP4_SYM = ${CCP4_SYM}." 

else if ( ${SYM} == 'p22121' ) then
  set spcgrp = "8"
  set spcgroup_num = "12"
  set docentric = "n"
  set ML_sym_val = "2"
  set CCP4_SYM = "18"
  set rotate_to_Z = "no"
  set TWOFOLD = "T"
  set verified = "yes"

else if ( ${SYM} == 'c222' ) then
  set spcgrp = "9"
  set spcgroup_num = "13"
  set CCP4_SYM = "21"
  set ML_sym_val = "2"
  set rotate_to_Z = "no"
  set TWOFOLD = "T"

else if ( ${SYM} == 'p4' ) then
  set spcgrp = "10"
  set spcgroup_num = "14"
  set CCP4_SYM = "75"
  set ML_sym_val = "4"
  set rotate_to_Z = "no"
  set verified = "yes"
  set set_to_90 = "yes"
  set TWOFOLD = "T"

else if ( ${SYM} == 'p422' ) then
  set spcgrp = "11"
  set spcgroup_num = "15"
  set CCP4_SYM = "89"
  set ML_sym_val = "4"
  set rotate_to_Z = "no"
  set verified = "yes"
  set set_to_90 = "yes"
  set TWOFOLD = "T"

else if ( ${SYM} == 'p4212' ) then
  set spcgrp = "12"
  set spcgroup_num = "16"
  set CCP4_SYM = "90"
  set ML_sym_val = "6"
  set rotate_to_Z = "no"
  set verified = "yes"
  set set_to_90 = "yes"
  set TWOFOLD = "T"

else if ( ${SYM} == 'p3' ) then
  set spcgrp = "13"
  set spcgroup_num = "17"
  set CCP4_SYM = "143"
  set ML_sym_val = "3"
  set rotate_to_Z = "no"
  set verified = "yes"
  set set_to_120 = "yes"

else if ( ${SYM} == 'p312' ) then
  set spcgrp = "14"
  set spcgroup_num = "18"
  set CCP4_SYM = "149"
  set ML_sym_val = "3"
  set rotate_to_Z = "no"
  set set_to_120 = "yes"

else if ( ${SYM} == 'p321' ) then
  set spcgrp = "15"
  set spcgroup_num = "19"
  set CCP4_SYM = "150"
  set ML_sym_val = "3"
  set rotate_to_Z = "no"
  set set_to_120 = "yes"

else if ( ${SYM} == 'p6' ) then
  set spcgrp = "16"
  set spcgroup_num = "20"
  set CCP4_SYM = "168"
  set ML_sym_val = "6"
  set rotate_to_Z = "no"
  set verified = "yes"
  set set_to_120 = "yes"
  set TWOFOLD = "T"

else if ( ${SYM} == 'p622' ) then
  set spcgrp = "17"
  set spcgroup_num = "21"
  set CCP4_SYM = "177"
  set ML_sym_val = "6"
  set rotate_to_Z = "no"
  set set_to_120 = "yes"
  set TWOFOLD = "T"

else
  ${proc_2dx}/protest "ERROR: sym2spcgrp: SYM of ${SYM} not supported"

endif
#
if ( ${verified} == "no" ) then
  ${proc_2dx}/linblock "WARNING: You are using a space group that is not yet verified."
  ${proc_2dx}/linblock "If this does not work, please submit a bug report to 2dx.org"
  ${proc_2dx}/linblock "If this does work, please give us feedback to 2dx.org."
  ${proc_2dx}/linblock "e.g. by using the bug-report tool, so that we can remove this message."
endif
#
echo ":sym2spcgrp: Input is SYM = ${SYM}"
echo ":sym2spcgrp: Output in spcgrp = ${spcgrp}"
echo ":sym2spcgrp: Output in spcgroup_num = ${spcgroup_num}"
echo ":sym2spcgrp: Output in CCP4_SYM = ${CCP4_SYM}"
echo ":sym2spcgrp: Output in ML_sym_val = ${ML_sym_val}"
echo ":sym2spcgrp: Output in rotate_to_Z = ${rotate_to_Z}"
echo ":sym2spcgrp: Output in TWOFOLD = ${TWOFOLD}"
#
if ( ${set_to_90} == "yes" ) then
  echo ":sym2spcgrp: Output in set_to_90 = ${set_to_90}"
  if ( ${realang}x != "90.0"x ) then
    set realang = 90.0
    echo "set realang = ${realang}" >> LOGS/${scriptname}.results
  endif
endif
if ( ${set_to_120} == "yes" ) then
  echo ":sym2spcgrp: Output in set_to_120 = ${set_to_120}"
  if ( ${realang}x != "120.0"x ) then
    set realang = 120.0
    echo "set realang = ${realang}" >> LOGS/${scriptname}.results
  endif
endif
#

