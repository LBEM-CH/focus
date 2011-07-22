The programs for 2-dimensional processing:


The main program for the ML on individual image  is in 2dx_ML.c, which calls main_ML.c

The main programs for the ML on merging multi-images: 2dx_ML_stack.c,  2dx_ML_merge.c

2dx_ML_stack calls:   wirte_stack_merge.c

2dx_ML_merge calls: main_ML_merge  




The programs for 3-dimensional processing:

2dx_merge/custom-script:   2dx_MaximumLikelihood.script --->  calls 2dx_ML_stack.exe   and 2dx_ML_merge.exe

2dx_ML_stack.c:  calls write_stack_merge_3D.c --> get_units.c, get_units_merge.c

2dx_ML_merge.c:  calls main_ML_merge_3D.c  --> maximum_likelihood_merge_3D.c (   call MLpthread.c  (ML) or CCpthread.c  (CC))

get_resolution_3D.c :  get the FSC resolution
 


