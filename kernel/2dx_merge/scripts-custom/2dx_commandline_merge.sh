#!/bin/sh

#set dirs
bin_2dx_merge=~/code/2dx/build/2dx/bin/2dx_merge
execution_dir=$PWD
working_dir1=/Users/marheit/Documents/2dx/CitS/CitS_NaCit_syn
working_dir2=/Users/marheit/Documents/2dx/CitS/CitS_NaAc_syn
merge_dir1="$working_dir1/merge"
merge_dir2="$working_dir2/merge"
merge_dir=$merge_dir1
tilt_dir=CitS00 
results_dir=/Users/marheit/Documents/2dx/CitS/diffmaps/results_minus


#functions
set_processing_param()
{
	if [ $# -lt 2 ]; then
		echo "usage: $0 param_name param_value "
		return 1
	fi
	_key=$1
	_value=$2
        if [ $# -lt 3 ]; then
	    config_file="$merge_dir/2dx_merge.cfg"
        else
	    config_file="$3"
	fi

	if [ ! -f $config_file ]; then
		echo "$config_file does not exist!"
		return 1
	fi
	line=`grep "set $_key = " $config_file`
	if [ $? -ne 0 ]; then
		echo "the key $_key is not present in $config_file."
		return 1
	fi	
	echo "setting $_key to $_value in $config_file"
        sed -i .bak "s/$line/set $_key \= \"$_value\"/g" $config_file
        grep "set $_key = " $config_file	
}

get_processing_param()
{
	if [ $# -lt 1 ]; then
		echo "usage: $0 param_name param_value "
		return 1
	fi
	_key=$1
        if [ $# -lt 2 ]; then
	    config_file="$merge_dir/2dx_merge.cfg"
        else
	    config_file="$2"
        fi
	if [ ! -f $config_file ]; then
		echo "$config_file does not exist!"
		return 1
	fi
	line=`grep "set $_key = " $config_file`
	if [ $? -ne 0 ]; then
		echo "the key $_key is not present in $config_file."
		return 1
	fi	
	_value=`echo $line | cut -d\" -f2`
	echo "$_key has the value $_value"
}

if [ ! -d $results_dir ]
then
	echo "results directory $results_dir does not exist"
	mkdir -p $results_dir
fi

if get_processing_param diff_map1_name; then
	map1_name=$_value
else
	map1_name="NaCitrate"
fi
if get_processing_param diff_map2_name; then
	map2_name=$_value
else
	map2_name="NaAcetate"
fi

noise2phaseres_file1="$merge_dir1/noise2phaseres_test.txt"
noise2phaseres_file2="$merge_dir2/noise2phaseres_test.txt"
#rm -rf $noise2phaseres_file1
rm -rf $noise2phaseres_file2


filebase=CitS00

NOISE=90
END=90
while [ $NOISE -le $END ]
do

	echo "NOISE LEVEL: $NOISE"


	#CONFORMATION1	
	conformation=CitS_NaCit
	cd $working_dir1
        if [ $NOISE -le 80 ]
        then
            if [ $NOISE -le 40 ]
            then
                filebase=`printf 'CitS0001%02d' $NOISE`
            else
                filebase=`printf 'CitS001%03d' $NOISE`
            fi
        else
            filebase=`printf 'Cits001%03d' $NOISE`
        fi
        echo $filebase
	selection=`find $tilt_dir -d 1 -name "${filebase}*"`
	cd $execution_dir
        echo "selection: $selection" 

	selection_file=selection.dat
	selection_filepath="$merge_dir1/$selection_file"
	rm -f $selection_filepath
	for image in $selection
	do
	   #     imagename=`echo $image | cut -d/ -f2`
           #     imagenumber=`echo "$imagename"  | sed 's/CitS//1'`
           #	set_processing_param  imagename "$imagename" $working_dir1/$image/2dx_image.cfg  
           #	set_processing_param  nonmaskimagename "$imagename" $working_dir1/$image/2dx_image.cfg  
           #	set_processing_param  imagenumber "$imagenumber" $working_dir1/$image/2dx_image.cfg  
		echo $image >>$selection_filepath
	done

	cat $selection_filepath
	# merge
	$bin_2dx_merge $working_dir1 2dx_finalmerge $selection_file
        if get_processing_param overall_phase_residual_2D; then
            phaseres=$_value
            echo "Phase Residual: $phaseres"
            echo "$NOISE $phaseres" >> $noise2phaseres_file1 
        fi
        # generate map
	$bin_2dx_merge $working_dir1 2dx_generateMergeMap $selection_file
	cp $merge_dir1/merge-p22121.mrc $results_dir/map-$conformation-$NOISE.mrc  
		
	conformation=CitS_NaAc
	cd $working_dir2
        if [ $NOISE -le 90 ]
        then
            filebase=`printf 'CitS000%03d' $NOISE`
        else
            filebase=`printf 'Cits000%03d' $NOISE`
        fi
        echo $filebase
	selection=`find $tilt_dir -d 1 -name "${filebase}*"`
	cd $execution_dir

	selection_file=selection.dat
	selection_filepath="$merge_dir2/$selection_file"
	rm -f $selection_filepath
	for image in $selection
	do
		echo $image >>$selection_filepath
	done

	cat $selection_filepath
	# merge
	$bin_2dx_merge $working_dir2 2dx_finalmerge $selection_file
        if get_processing_param overall_phase_residual_2D "$merge_dir2/2dx_merge.cfg"; then
            phaseres=$_value
            echo "Phase Residual: $phaseres"
            echo "$NOISE $phaseres" >> $noise2phaseres_file2 
        fi
	
        # generate map
	$bin_2dx_merge $working_dir2 2dx_generateMergeMap $selection_file
	cp $merge_dir2/merge-p22121.mrc $results_dir/map-$conformation-$NOISE.mrc  

	#DIFFMAP
	#raw difference map
	set_processing_param  diffmap_det_significance "n" 
	#plot the map without title etc
	set_processing_param diffmap_plot_clean "y" 
	set_processing_param diffmap_plot_scalebar "y" 
	$bin_2dx_merge $working_dir1 +2dx_diffmapII $selection_file
	
	diffmap_dir="$merge_dir1/diffmap"
	if [ ! -d $diffmap_dir ]; then
		echo "diffmap directory $diffmap_dir does not exist!"
		exit 1
	fi
	cd $diffmap_dir
        find . -d 1 -name "diffmap*" -exec cp {} "$results_dir/diffmap_raw_${NOISE}_${map1_name}-${map2_name}.pdf" \;
	cp ${map1_name}.pdf "$results_dir/${map1_name}_${NOISE}.pdf"  	
	cp ${map2_name}.pdf "$results_dir/${map2_name}_${NOISE}.pdf"  	
	cd $execution_dir	
		
#	#determine significant changes
	set_processing_param diffmap_det_significance "y" 
	set_processing_param diffmap_threshold_global "n" 
	set_processing_param diffmap_threshold_method "max" 
	#classic method
	diffmap_method="Classic"
	set_processing_param diffmap_sig_algorithm "$diffmap_method" 
	$bin_2dx_merge $working_dir1 +2dx_diffmapII $selection_file
	cd $diffmap_dir
        find . -d 1 -name "diffmap*.pdf" -exec cp {} "$results_dir/diffmap_${diffmap_method}_${NOISE}_${map1_name}-${map2_name}.pdf" \;
        find . -d 1 -name "varmap*.pdf" -exec cp {} "$results_dir/varmap_${diffmap_method}_${NOISE}_${map1_name}-${map2_name}.pdf" \;
	cd $execution_dir

	set_processing_param diffmap_threshold_method "rmsd" 
	set_processing_param diffmap_threshold_global "y" 
	set_processing_param diffmap_var_factor "2.0" 
	$bin_2dx_merge $working_dir1 +2dx_diffmapII $selection_file
	cd $diffmap_dir
        find . -d 1 -name "diffmap*" -exec cp {} "$results_dir/diffmap_${diffmap_method}_rmsd2_${NOISE}_${map1_name}-${map2_name}.pdf" \;
	cd $execution_dir	
	set_processing_param diffmap_var_factor "1.0" 
       
       
        set_processing_param diffmap_threshold_method "rmsd" 
	set_processing_param diffmap_threshold_global "n" 
	$bin_2dx_merge $working_dir1 +2dx_diffmapII $selection_file
	cd $diffmap_dir
        find . -d 1 -name "diffmap*" -exec cp {} "$results_dir/diffmap_${diffmap_method}_rmsd_${NOISE}_${map1_name}-${map2_name}.pdf" \;
	cd $execution_dir	


	set_processing_param diffmap_threshold_method "mean" 
	set_processing_param diffmap_threshold_global "y" 
	$bin_2dx_merge $working_dir1 +2dx_diffmapII $selection_file
	cd $diffmap_dir
        find . -d 1 -name "diffmap*" -exec cp {} "$results_dir/diffmap_${diffmap_method}_mean_${NOISE}_${map1_name}-${map2_name}.pdf" \;
	cd $execution_dir	

	set_processing_param diffmap_threshold_method "max" 
	set_processing_param diffmap_threshold_global "y" 
	$bin_2dx_merge $working_dir1 +2dx_diffmapII $selection_file
	cd $diffmap_dir
        find . -d 1 -name "diffmap*" -exec cp {} "$results_dir/diffmap_${diffmap_method}_max_${NOISE}_${map1_name}-${map2_name}.pdf" \;
	cd $execution_dir	
	set_processing_param diffmap_threshold_global "n" 
	set_processing_param diffmap_threshold_method "max" 

		
	#mixed merge method
	diffmap_method="Mixed Merge"
	set_processing_param diffmap_sig_algorithm "$diffmap_method" 
	$bin_2dx_merge $working_dir1 +2dx_diffmapII $selection_file
	diffmap_method="Mixed_Merge"
	cd $diffmap_dir
        find . -d 1 -name "diffmap*" -exec cp {} "$results_dir/diffmap_${diffmap_method}_${NOISE}_${map1_name}-${map2_name}.pdf" \;
        find . -d 1 -name "varmap*.pdf" -exec cp {} "$results_dir/varmap_${diffmap_method}_${NOISE}_${map1_name}-${map2_name}.pdf" \;
	cd $execution_dir


	set_processing_param diffmap_threshold_method "rmsd" 
	set_processing_param diffmap_threshold_global "y" 
	set_processing_param diffmap_var_factor "2.0" 
	$bin_2dx_merge $working_dir1 +2dx_diffmapII $selection_file
	cd $diffmap_dir
        find . -d 1 -name "diffmap*" -exec cp {} "$results_dir/diffmap_${diffmap_method}_rmsd2_${NOISE}_${map1_name}-${map2_name}.pdf" \;
	cd $execution_dir	
	set_processing_param diffmap_var_factor "1.0" 
	
        set_processing_param diffmap_threshold_method "rmsd" 
	set_processing_param diffmap_threshold_global "n" 
	$bin_2dx_merge $working_dir1 +2dx_diffmapII $selection_file
	cd $diffmap_dir
        find . -d 1 -name "diffmap*" -exec cp {} "$results_dir/diffmap_${diffmap_method}_rmsd_${NOISE}_${map1_name}-${map2_name}.pdf" \;
	cd $execution_dir	


	
        set_processing_param diffmap_threshold_method "mean" 
	set_processing_param diffmap_threshold_global "y" 
	$bin_2dx_merge $working_dir1 +2dx_diffmapII $selection_file
	cd $diffmap_dir
        find . -d 1 -name "diffmap*" -exec cp {} "$results_dir/diffmap_${diffmap_method}_mean_${NOISE}_${map1_name}-${map2_name}.pdf" \;
	cd $execution_dir	

	set_processing_param diffmap_threshold_method "max" 
	set_processing_param diffmap_threshold_global "y" 
	$bin_2dx_merge $working_dir1 +2dx_diffmapII $selection_file
	cd $diffmap_dir
        find . -d 1 -name "diffmap*" -exec cp {} "$results_dir/diffmap_${diffmap_method}_max_${NOISE}_${map1_name}-${map2_name}.pdf" \;
	cd $execution_dir	
       set_processing_param diffmap_threshold_global "n" 
       set_processing_param diffmap_threshold_method "max" 



       #student's t-test method
	diffmap_method="Student's t-test"
	set_processing_param diffmap_sig_algorithm "$diffmap_method" 
	$bin_2dx_merge $working_dir1 +2dx_diffmapII $selection_file
	cd $diffmap_dir
	diffmap_method="Student"
        find . -d 1 -name "diffmap*" -exec cp {} "$results_dir/diffmap_${diffmap_method}_${NOISE}_${map1_name}-${map2_name}.pdf" \;
       find . -d 1 -name "varmap*.pdf" -exec cp {} "$results_dir/varmap_${diffmap_method}_${NOISE}_${map1_name}-${map2_name}.pdf" \;
	cd $execution_dir
	
	#welch's t-test method
	diffmap_method="Welch's t-test"
	set_processing_param diffmap_sig_algorithm "$diffmap_method" 
	$bin_2dx_merge $working_dir1 +2dx_diffmapII $selection_file
	cd $diffmap_dir
	diffmap_method="Welch"
        find . -d 1 -name "diffmap*" -exec cp {} "$results_dir/diffmap_${diffmap_method}_${NOISE}_${map1_name}-${map2_name}.pdf" \;
	cp ${map1_name}.pdf "$results_dir/${map1_name}_${diffmap_method}_${NOISE}.pdf"  	
	cp ${map2_name}.pdf "$results_dir/${map2_name}_${diffmap_method}_${NOISE}.pdf"  	
	cd $execution_dir
	
	# Next noise level
	((NOISE = NOISE + 10))
done

