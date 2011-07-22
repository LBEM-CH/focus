/*  Generate stacks from multiple crystals that are included in the dirfile.dat.
 *
 *  Created by Xiangyan Zeng on 1/1/2007
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <fftw3.h>
#include <string.h>
#include <mrcImage.h>

int  rmax1=5, rmax2=60; 

/*   All the parameters used as common variables  */ 

float  threshold, relative_threshold, Terminate_ML,x_center, y_center,lp_radius, min_angle, max_angle,step_angle;
float  B_fac,A;
int    mask_radius,Iteration, ref_ind, Symmetry=4,threshold_method,lp_method;
int    realcell_x1, realcell_y1, realcell_x, realcell_y, oversized_x,oversized_y, realcell_x1_common, realcell_y1_common;
int    doMLorCC,contrast;
int do_whiten,correct_CTF;
char   resultsfilename[200];


#include "string_to_real.c"
#include "string_to_integer.c"
#include "cgetline.c"
#include "fgetline.c"
#include "igetline.c"
#include "trans.c"
#include "quadintp.c"
#include "rotate.c"
#include "cross_corr.c" 
 

#include "write_stack_merge_3D.c"
         

int main(int argc, char **argv)
{ 
  int dir,m,i,j,k,n,num_line;
  int num_images, Nimages=1, *pa, sx;   /*  Prepare for merge Nimages images together into a stack of num_images patches*/
  int oversizexy[2], realcellxy[2], realcellxy1[2];
  float angle[3];

  char *c; 
  

  printf("\n2dx_ML: Performs Maximum Likelihood processing. \n");
  fflush(stdout);     
 
  /*  input global parameters from 2dx_image.cfg to generate stacks*/
 
  if(argc!=1)
  {
    printf("\t usage: 2dx_ML.exe\n");
    printf("\t example: 2dx_ML.exe\n"); 
    exit(1);
  }
    
   
/*   ML main program */
 
        FILE *input,*output;
         
        
	char *dirname,  *str;
 	int start, end;

	dirname=(char *)calloc(200,sizeof(char));
	str=(char *)calloc(200,sizeof(char));
	
	printf("::prepare stacks for ML \n\n");
	printf("Input parameters common to all stacks \n");
        fflush(stdout);	

 
       /*   Input common parameters from 2dx_merge.cfg  */
 
        input=fopen("2dx_merge.cfg","r");
        if(input==NULL) 
       {
             printf("::2dx_merge.cfg does not exist.\n");
             exit(0);
	}
 	/*
        if(igetline(input, "ML_oversizexy", oversizexy)==0)  
               {   oversized_x=oversizexy[0];
                     oversized_y=oversizexy[1];
                     printf("ML_oversizexy................. = %d,%d\n",oversized_x,oversized_y);
              } 
	else printf("parameter ML_oversizexy does not exists \n");   
        */
         
        if(*(cgetline(input,"ML_do_whiten"))=='y')
            {            do_whiten =1;
                          printf("do_whiten...................... = yes \n");
             }
	   else
            {             do_whiten=0;
                            printf("do_whiten...................... = no \n");
             }
 
	if(*(cgetline(input,"ML_correct_CTF"))=='y')
             {     correct_CTF =1;
                    printf("correct_CTF.................... = yes \n");
             }
	 else
            {    correct_CTF=0;
                  printf("correct_CTF.................... = no \n");
            }
  
         
        if(igetline(input, "ML_realcellxy_outer", realcellxy)==0)  
            {   realcell_x=realcellxy[0];
                 realcell_y=realcellxy[1];
                 printf("ML_realcellxy_outer............ = %d,%d\n",realcell_x,realcell_y);
            }
         else printf("parameter ML_realcellxy_outer does not exists \n");    
         
	 
    	  
	if(igetline(input, "ML_realcellxy_inner", realcellxy1)==0)  
	  {   realcell_x1_common=realcellxy1[0];
               realcell_y1_common=realcellxy1[1];
               printf("ML_realcellxy_inner........... = %d,%d\n",realcell_x1_common,realcell_y1_common);
          } 
         else printf("parameter ML_realcellxy_inner does not exists \n");    
	  
//	  realcell_x1=realcell_x;
//	  realcell_y1=realcell_y;
	  
        if(igetline(input, "ML_mask_radius", &mask_radius)==0) 
             printf("Mask radius................... = %d \n",mask_radius); 
	else printf("parameter ML_mask_radius does not exists \n"); 
 
	if(igetline(input, "ML_threshold_method", &threshold_method)==0) 
             {     
                   if(threshold_method==0)
                       {   
                             if(fgetline(input, "ML_absolute_threshold", &threshold)==0) 
                                        printf("Absolute Threshold value....... = %f \n",threshold);
                             else  printf("parameter ML_absolute_threshold does not exists \n");     
                       }  
                   else  
                     {     if(fgetline(input, "ML_relative_threshold", &relative_threshold)==0)    
                                      printf("Percentage of peaks to use..... = %.2f \n",relative_threshold);
                            else printf("parameter ML_relative_threshold does not exists \n");
                     }
              }
	 else printf("parameter ML_threshold_method does not exists \n");   
  
  fflush(stdout);
  
 
 /*
	if(igetline(input, "ML_DS_ratio", &DS_ratio)==0) 
               printf("DS_ratio...................... = %d \n",DS_ratio); 
         else printf("parameter DS_ratio does not exists \n");  
*/    
	fclose(input);

  
 
        /*   Get particles from multiple crystals */
 
	input=fopen("2dx_merge_dirfile.dat","r"); 
	if(input==NULL) 
	{
		printf("::2dx_merge_dirfile.dat does not exist.\n");
		exit(0);
	}
	k=0;
        output=fopen("2dx_merge_dirfile_temp.dat","w");

	char *read=fgets(str, 200, input);	

	while(feof(input)==0)  
	{   	 
		end=strlen(str);

		for(i=0;i<end-1;i++)
			dirname[i]=str[i];
                dirname[i]='/';
                
		if(write_stack_merge(k, dirname)==0)
		 {		 
		      fputs(str, output);
		      
		        k++;
                 }
                 
                 for(i=0;i<200;i++)
			dirname[i]='\0';
                
		read=fgets(str, 200, input);                
	}

	fclose(input);  
        fclose(output);
        
        free(dirname);
        free(str);

	printf("::2dx_ML_stack  finished correctly with %d stacks\n\n",k);

	return 0;


}
