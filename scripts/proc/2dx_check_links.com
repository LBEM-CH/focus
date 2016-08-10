########################################################################
#
# SCRIPT: 2dx_check_links.com
# BRIEF: Checks existence of all the required links and config files  in the 
#        directories selected and stops if not the case. 
# USAGE:
#	2dx_check_links.com <merge_dir>
#
# where <merge_dir> is the path where all the merge files are kept.
#
# AUTHOR: Nikhil Biyani
#         Henning Stahlberg
#
########################################################################
#
set merge_dir=$1
#
#
set linkok = "y"
#
#
cd ${merge_dir}/../
echo "Checking link in project root directory \n ${PWD}:"
if ( ! -e 2dx_master.cfg ) then
    echo ":: "
    echo "::WARNING: 2dx_master.cfg in project root directory is missing."
    echo ":: "
    set linkok = "n"
else
    \ls -l 2dx_master.cfg
    if ( ! -l 2dx_master.cfg ) then
        echo ":: "
        echo "::WARNING: 2dx_master.cfg is not a link in ${PWD}."
        echo ":: "
        set linkok = "n"
    endif
endif
cd ${merge_dir}
#
#
#----------------------------------------------------------------------
${proc_2dx}/linblock "Checking the links of selected image directories"
#----------------------------------------------------------------------
#
set dirlistfile = "${merge_dir}/2dx_merge_dirfile.dat"
#
if ( -s ${dirlistfile} ) then
    set coutfile = "${merge_dir}/SCRATCH/2dx_getdirectories_tmp.dat"
    \rm -f ${coutfile}
    #
    ${bin_2dx}/2dx_getdirectories.exe << eot
${merge_dir}/2dx_merge_dirfile.dat
${merge_dir}
${coutfile}
eot
    #
    cat ${coutfile} | tr "\n" " " > ${merge_dir}/SCRATCH/2dx_getdirectories_tmp.dat
    set dirlist = "`cat ${merge_dir}/SCRATCH/2dx_getdirectories_tmp.dat`"
    #
    foreach dirfile ( ${dirlist} ) 
        echo "Checking link in ${dirfile}"
        cd ${merge_dir}
        cd ..
        if ( -d ${dirfile} ) then
            cd ${dirfile}
            if ( ! -e 2dx_master.cfg ) then
                echo ":: "
                echo "::WARNING: 2dx_master.cfg is missing in ${PWD}."
                echo ":: "
                set linkok = "n"
            else
                \ls -l 2dx_master.cfg
                if ( ! -l 2dx_master.cfg ) then
                    echo ":: "
                    echo "::WARNING: 2dx_master.cfg is not a link in ${PWD}."
                    echo ":: "
                    set linkok = "n"
                endif
            endif

            cd ${merge_dir}
        else
            set linkok = "n"
        endif
    end
endif
#
if ( ${linkok} == 'n' ) then
    echo ":: "
    echo ":: WARNING: Your project has not the correct file system structure."
    echo ":: The files 2dx_master.cfg in the tilt-range directories should be links"
    echo ":: to the one-level higher version, which should be a link to the one in the"
    echo ":: merge directory."
    echo ":: "
    echo ":: Run the Custom Script Repair Project Links to repair this."
    echo ":: "
endif
#