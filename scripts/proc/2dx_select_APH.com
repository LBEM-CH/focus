###############################################################################
#
# SCRIPT: 2dx_select_APH.com
# BRIEF: Chooses the correct APH file to be used depending on the choice of user
#        provided by variable ${user_data_type}
#        Choices are:
#        0: Ubent II APH file
#        1: Movie Mode A
#        2: Movie Mode B
#        3: Maximum Likelihood Single Particle
#        4: Best of Above
#        5: Use all in case of final merge/ Otherwise Best of above
#
# OUTPUT: Copies the correct APH file to "image_ctfcor_ctf.aph" 
#
# USAGE:
#	2dx_select_APH.com  <merge_dir> <user_data_type>
#
# AUTHOR: Nikhil Biyani
#         Henning Stahlberg
#
###############################################################################
#
set merge_dir=${1}
set user_data_type=${2}
#
#----------------------------------------------------------------------
# {proc_2dx}/linblock "Setting APH file types to use"
#----------------------------------------------------------------------
#
cd ${merge_dir}
foreach imagefile ( ${dirlist} )
    cd ..
    cd ${imagefile}
    if ( ! -e 2dx_image.cfg ) then
        ${proc_2dx}/protest "ERROR: 2dx_image.cfg not found in ${imagefile}"
    endif
    
    cd APH
    set APH_file = image_ctfcor_fou_unbent_ctf.aph
    if ( ${user_data_type} == '0' ) then
        set APH_file = image_ctfcor_fou_unbent_ctf.aph
    endif

    if ( ${user_data_type} == '1' ) then
        if ( -e image_ctfcor_movie_fou_ctf.aph ) then
            set APH_file = image_ctfcor_movie_fou_ctf.aph
        else
            echo ":image_ctfcor_movie_fou_ctf.aph not found."
        endif
    endif

    if ( ${user_data_type} == '2' ) then 
        if ( -e image_ctfcor_movieB_fou_ctf.aph ) then
            set APH_file = image_ctfcor_movieB_fou_ctf.aph
        else
            echo ":image_ctfcor_movieB_fou_ctf.aph not found."
        endif
    endif

    if ( ${user_data_type} == '3' ) then
        if ( -e ML_result.aph ) then
            set APH_file = ML_result.aph
        else
            echo ":ML_result.aph not found."
        endif
    endif

    if (${user_data_type} == '4' || ${user_data_type} == '5' ) then
        set QVAL2_local =  `cat ../2dx_image.cfg | grep 'set QVAL2 =' | cut -d\" -f2`
        set QVALS_local =  `cat ../2dx_image.cfg | grep 'set QVALS =' | cut -d\" -f2`
        if ( ${QVALS_local} == '.' ) then
            set QVALS_local = 0
        endif
 
        set QVALMA_loctmp =  `cat ../2dx_image.cfg | grep 'set QVALMA =' | cut -d\" -f2`
        if ( ${QVALMA_loctmp} == '.' ) then
            set QVALMA_loctmp = 0
        endif
 
        set QVALMB_loctmp =  `cat ../2dx_image.cfg | grep 'set QVALMB =' | cut -d\" -f2`
        if ( ${QVALMB_loctmp} == '.' ) then
            set QVALMB_loctmp = 0
        endif
 
        set QVALMA_local  = `echo ${QVALMA_loctmp} 1.1 | awk '{ s = $1 * $2 } END { print s }'`
        set QVALMB_local  = `echo ${QVALMB_loctmp} 1.1 | awk '{ s = $1 * $2 } END { print s }'`

        echo ${QVAL2_local} > awk.dat
        echo ${QVALMA_local} >> awk.dat
        echo ${QVALMB_local} >> awk.dat

        #
        # awk script to find max in CR-separated list:
        #  {if(min=="")min=max=$1}; if($1>max) {max=$1}; if($1< min) {min=$1}; total+=$1; count+=1} END {print total/count, min, max}
        #
        set QVAL_max = `awk '{if(max==""){max=$1;best=1;count=1}; if($1>max) {max=$1;best=count}; count += 1} END {print max}' awk.dat`
        set QVAL_best = `awk '{if(max==""){max=$1;best=1;count=1}; if($1>max) {max=$1;best=count}; count += 1} END {print best}' awk.dat`
        \rm -f awk.dat
   
        if ( ${QVAL_best} == '1' ) then
            set APH_file = image_ctfcor_fou_unbent_ctf.aph
        endif

        if ( ${QVAL_best} == '2' ) then
            set APH_file = image_ctfcor_movie_fou_ctf.aph
        endif
     
        if ( ${QVAL_best} == '3' ) then
            set APH_file = image_ctfcor_movieB_fou_ctf.aph
        endif
    endif

    if ( ! -e ${APH_file} ) then
        echo "::${APH_file} should be best, but was not found in $PWD."
        set APH_file = image_ctfcor_fou_unbent_ctf.aph
    else
       echo ":In directory ${imagefile}: Using APH file ${APH_file}. "
       \rm -f image_ctfcor_ctf.aph
       \ln -s ${APH_file} image_ctfcor_ctf.aph
    endif
    cd ${merge_dir}
end
#
