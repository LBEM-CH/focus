#
#
# this is not an independent script. It should only be called from other scripts.
#
# This sub-script will echo a variable into the logfile
#
set ${reset_var} = "${reset_val}"
echo "set ${reset_var} = "\"${reset_val}\" >> LOGS/${scriptname}.results
#

