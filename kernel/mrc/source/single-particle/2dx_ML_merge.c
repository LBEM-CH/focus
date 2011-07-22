/*  Merge multiple crystals stored in stacks
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

int  rmax1=5, rmax2=60, rmask=40;
 

/*   All the parameters used as common variables  */ 

float  threshold, relative_threshold, Terminate_ML,x_center, y_center,lp_radius, min_angle, max_angle,step_angle;
float  B_fac,A;
int    mask_radius,Iteration, ref_ind, Symmetry,threshold_method,lp_method;
int    realcell_x1, realcell_y1, realcell_x, realcell_y, oversized_x,oversized_y, realcell_x1_common, realcell_y1_common;
int    doMLorCC,contrast;



#include "string_to_real.c"
#include "string_to_integer.c"
#include "cgetline.c"
#include "fgetline.c"
#include "igetline.c"
#include "normalize_image.c"
#include "main_ML_merge_3D.c"            /*  ML using whitening of the whole image (Niko's algorithm)  */  
 
int main(int argc, char **argv)
{ 
	int dir,m,i,j,k,n,num_line, Symmetry1;
        int num_images, Nimages=1, *pa,sx;   /*  Prepare for merge Nimages images together into a stack of num_images patches*/
        int oversizexy[2], realcellxy[2], realcellxy1[2];
        float angle[3];
        char   *resultsfilename;
        resultsfilename=(char *)calloc(200,sizeof(char));


	FILE *input, *output,*directoryfile;
        char oneline[200];
        char *c; 
  

 	int read=scanf("%s",resultsfilename);
 	printf("Results File Name............. = %s \n",resultsfilename);


	printf("\n2dx_ML: Performs Maximum Likelihood processing. \n");
          
    
	input=fopen("2dx_merge.cfg" ,"r"); 
        if(input==NULL) 
        {
             printf("\n  2dx_merge.cfg does not exist.\n");
             exit(1);
         }

   /*  input  parameters from 2dx_merge.cfg to run maximum likelihood */
  
  	if(argc!=1)
  	{
   	     printf("\t usage: 2dx_ML.exe\n");
             printf("\t example: 2dx_ML.exe\n"); 
             exit(1);
         }
  
 	printf("\nReading  Parameters for ML:\n");
  
        /*
	if(igetline(input, "ML_oversizexy", oversizexy)==0)  
 	  {    oversized_x=oversizexy[0];
       		oversized_y=oversizexy[1];
                printf("ML_oversizexy................. = %d,%d\n",oversized_x,oversized_y);
           } 
        else printf("parameter ML_oversizexy does not exists \n");   
        */
        
         if(igetline(input, "ML_realcellxy_outer", realcellxy)==0)  
	 {   realcell_x=realcellxy[0];
              realcell_y=realcellxy[1];
              printf("ML_realcellxy_outer........... = %d,%d\n",realcell_x,realcell_y);
         }
	else printf("parameter ML_realcellxy_outer does not exists \n");    
    
		  
	if(igetline(input, "ML_realcellxy_inner", realcellxy1)==0)  
	  {   	realcell_x1_common=realcellxy1[0];
               realcell_y1_common=realcellxy1[1];
               printf("ML_realcellxy_inner........... = %d,%d\n",realcell_x1_common,realcell_y1_common);
          } 
         else printf("parameter ML_realcellxy_inner does not exists \n");   
     
	  
//	  realcell_x1=realcell_x;
//	  realcell_y1=realcell_y;
	  
        if(igetline(input, "ML_mask_radius", &mask_radius)==0) 
             printf("Mask radius................... = %d \n",mask_radius); 
	else printf("parameter ML_mask_radius does not exists \n"); 
	

	if(igetline(input, "ML_iteration", &Iteration)==0) 
                printf("Iteration .................... = %d \n",Iteration); 
        else printf("parameter ML_iteration does not exists \n");   
  
	if(igetline(input, "ML_ref_ind", &ref_ind)==0) 
	 {    
	       if ( ref_ind == 0 ) 
	          printf("Initial Reference (ref_ind)... = average reference \n");
               else if ( ref_ind == 1 ) 
                      printf("Initial Reference (ref_ind)... = random noise \n");
               else if ( ref_ind == 2 )
                     printf("Initial Reference (ref_ind)... = random particle \n");
               else
                    printf("::Initial Reference (ref_ind)... = ILLEGAL CHOICE !!!! \n");
           }
	else printf("parameter ML_ref_ind does not exists \n");   
 
 /*
	if(igetline(input, "ML_DS_ratio", &DS_ratio)==0) 
                printf("DS_ratio...................... = %d \n",DS_ratio); 
        else printf("parameter DS_ratio does not exists \n");  
*/
	if(igetline(input, "ML_rotational_symmetry", &Symmetry1)==0) 
	{   if(Symmetry1==0) Symmetry=1;
	     else if(Symmetry1==1) Symmetry=2;
	     else if(Symmetry1==2) Symmetry=3;
	     else if(Symmetry1==3) Symmetry=4;
	     else if(Symmetry1==4) Symmetry=6;
             printf("Symmetry1=%d   Symmetry...................... = %d \n", Symmetry1, Symmetry); 
        }
	else printf("parameter Symmetry does not exists \n");  
  
	if(igetline(input, "ML_lp_method", &lp_method)==0) 
        {   
              if(lp_method==1)
                    printf("Low-Pass filter............... = Gaussian \n");
              else if(lp_method==2)
                    printf("Low-Pass filter................ = Sharp cutoff \n");
              else
                   printf("Low-Pass filter............... = None \n");
        }
	else printf("parameter ML_lp_method does not exists \n");   
   
	if(fgetline(input, "ML_MinMaxStep_angle", angle)==0)
         { 
              min_angle=angle[0];
              max_angle=angle[1];
              step_angle=angle[2];
              printf("min_angle,max_angle,step_angle = %f,%f,%f \n",min_angle, max_angle, step_angle);   
          }
         else printf("Angle parameters do not exists \n"); 

        if(fabs(step_angle)<1.0e-3)
        {    printf("\n ERROR: angle step cannot be zero \n");
             fflush(stdout);
             exit(1);     
        }
        if(min_angle>max_angle)
        {    printf("\n ERROR:  max_angle should be larger than min_angle \n");
             fflush(stdout);
             exit(1);
        }  

	if(fgetline(input,"ML_terminate_ML",&Terminate_ML)==0)
               printf("Terminate_ML.................. = %f \n",Terminate_ML); 
	else printf("paramter ML_terminate does not exists \n");
  
	if(fgetline(input,"ML_lp_radius",&lp_radius)==0)
               printf("lp_radius..................... = %f \n",lp_radius); 
        else printf("paramter lp_radius does not exists \n");

	if(fgetline(input,"ML_B_factor",&B_fac)==0)
              printf("B_factor...................... = %f \n",B_fac); 
	else printf("paramter B_factor does not exists \n");
  
	if(fgetline(input,"ML_A_factor",&A)==0)
             printf("A_factor...................... = %f \n",A); 
 	else printf("paramter A_factor does not exists \n");
 
	if(*(cgetline(input,"ctfrev"))=='y')
        {    contrast=-1;
             printf("Contrast of final map......... = positive\n");
        }
	else
        {  contrast=1;
           printf("Contrast of final map......... = negative\n");
        }
 
	printf("\n::Starting ML\n\n");
	fclose(input); 
    

/*   ML main program */
 
  
	char *dirfilename, *str;
 	int start, end;

	dirfilename=(char *)calloc(200*1000,sizeof(char));
	str=(char *)calloc(200,sizeof(char));
     
	input=fopen("2dx_merge_dirfile_temp.dat","r"); 
        if(input==NULL) 
        {
             printf("::2dx_merge_dirfile_temp.dat does not exist.\n");
             exit(1);
         }
 	k=0;
 
	char *readCh=fgets(str, 200, input);	
	
	while(feof(input)==0)  
	  {   
	        strcat(str,"/");
                end=strlen(str);     
       
                for(j=0;j<end;j++)
                    dirfilename[j+k*200]=str[j]; 
               dirfilename[j+k*200]='/';
                k++;
 
                readCh=fgets(str, 200, input);
                  
           }

	fclose(input);  

       printf("::starting ML on %d  stacks   \n\n  ", k);

       ML(k, dirfilename,resultsfilename);
  
       printf("::2dx_ML finished correctly\n\n");
       
       free(resultsfilename);
       free(str);
       free(dirfilename);
      
       return 0;
}
