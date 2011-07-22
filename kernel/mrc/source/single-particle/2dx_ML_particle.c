/*
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

 

/*   All the parameters used as common variables  */ 
float  CS, KV, RESMIN, RESMAX, DIFMID1, DIFMID2, ANGAST, DSTEP, XMAG;
float  threshold, relative_threshold, Terminate_ML,x_center, y_center,lp_radius, min_angle, max_angle,step_angle;
float  B_fac,A,lattice[4],phaori[2];
int    mask_radius,Iteration, ref_ind, correct_CTF,Symmetry,threshold_method,lp_method;
int    realcell_x1, realcell_y1, realcell_x, realcell_y, oversized_x,oversized_y,do_whiten, DS_ratio;
int    doMLorCC,contrast;
char   imagename[200],profilename[200],resultsfilename[200],imagenumber[200];


// #include "main_ML.c"           /*  ML using whitening of the whole image (Niko's algorithm)  */  
#include "main_ML_particle.c"     /*  ML using whitening of individual particle */
  

int main(int argc, char **argv)
{ int dir,m,i,num_line;
  char oneline[100];
  char *c; 
  
  printf("\n2dx_ML: Performs Maximum Likelihood processing. \n");

  if(argc!=1)
  {
    printf("\t usage: 2dx_ML.exe\n");
    printf("\t example: 2dx_ML.exe\n"); 
    exit(1);
  }
  
  printf("\nReading Parameter:\n");
   

  scanf("%d",&doMLorCC);
  if(doMLorCC==0)
    printf("Weights for ref calculation... = from Maximum Likelihood estimate\n");
  else
    printf("Weights for ref calculation... = from Cross-Correlation between particle and old reference\n");

  scanf("%s",imagename);
  printf("Image Name.................... = %s \n",imagename);
  scanf("%s",imagenumber);
  printf("Image Number.................. = %s \n",imagenumber);
  scanf("%s",profilename);
  printf("Profile Name.................. = %s \n",profilename);
  scanf("%s",resultsfilename);
  printf("Results File Name............. = %s \n",resultsfilename);
  scanf("%f ",&CS  ); 
  printf("CS............................ = %f  \n",CS); 
  scanf("%f  ",&KV  ); 
  printf("KV............................ = %f  \n",KV); 
  scanf("%f  ",&RESMIN); 
  printf("RESMIN........................ = %f  \n",RESMIN); 
  scanf("%f  ",&RESMAX); 
  printf("RESMAX........................ = %f  \n",RESMAX); 
  scanf("%f %f %f",&DIFMID1, &DIFMID2, &ANGAST  ); 
  printf("DFMID1,DFMID2,ANGAST.......... = %f,%f,%f\n",DIFMID1,DIFMID2, ANGAST); 
  scanf("%f",&DSTEP  ); 
  printf("DSTEP......................... = %f\n",DSTEP); 
  scanf("%f",&XMAG); 
  printf("Magnification (XMAG).......... = %f \n",XMAG);  
  scanf("%d %d",&oversized_x, &oversized_y); 
  printf("oversized_x,oversized_y....... = %d,%d \n",oversized_x, oversized_y ); 
  scanf("%d %d",&realcell_x1, &realcell_y1); 
  printf("realcell_x1,realcell_y1....... = %d,%d \n",realcell_x1, realcell_y1 ); 
  scanf("%d %d",&realcell_x, &realcell_y); 
  printf("realcell_x,realcell_y......... = %d,%d \n",realcell_x, realcell_y); 
  scanf("%d",&Iteration); 
  printf("Iteration..................... = %d \n",Iteration); 
  scanf("%f",&threshold_method);
  if(threshold_method==1)
    printf("Threshold Method.............. = Absolute Value\n");
  else
    printf("Threshold Method.............. = Relative\n");
  scanf("%f",&threshold);
  printf("Absolute Threshold value...... = %f \n",threshold);
  scanf("%f",&relative_threshold);
  printf("Percentage of peaks to use.... = %.2f% \n",relative_threshold);

  //scanf("%f %f",&x_center, &y_center); 
  //printf("x_center,y_center............. = %f,%f \n",x_center,y_center); 

  scanf("%d",&mask_radius);
  printf("mask_radius................... = %d \n",mask_radius);
  scanf("%d",&ref_ind);
  if ( ref_ind == 0 )
    printf("Initial Reference (ref_ind)... = average reference \n");
  else if ( ref_ind == 1 )
    printf("Initial Reference (ref_ind)... = random noise \n");
  else if ( ref_ind == 2 )
    printf("Initial Reference (ref_ind)... = random particle \n");
  else
    printf("::Initial Reference (ref_ind)... = ILLEGAL CHOICE !!!! \n");
  scanf("%d",&DS_ratio);
  printf("DS_ratio...................... = %d \n",DS_ratio);
  scanf("%d",&do_whiten);
  if ( do_whiten == 1 )
    printf("do_whiten..................... = yes \n");
  else
    printf("do_whiten..................... = no \n");
  scanf("%d",&correct_CTF);
  if ( correct_CTF == 1 )
    printf("correct_CTF................... = yes \n");
  else
    printf("correct_CTF................... = no \n");

  scanf("%d",&Symmetry);
  printf("Symmetry...................... = %d \n",Symmetry);
  scanf("%f %f %f",&min_angle, &max_angle, &step_angle);
  printf("min_angle,max_angle,step_angle = %f,%f,%f \n",min_angle, max_angle, step_angle);
  scanf("%f",&Terminate_ML);
  printf("Terminate_ML.................. = %f \n",Terminate_ML);

  scanf("%d",&lp_method);
  if(lp_method==1)
    printf("Low-Pass filter............... = Gaussian \n");
  else if(lp_method==2)
    printf("Low-Pass filter............... = Sharp cutoff \n");
  else
    printf("Low-Pass filter............... = None \n");

  scanf("%f",&lp_radius);
  printf("Low-Pass radius............... = %f \n",lp_radius);

  scanf("%f",&B_fac);
  printf("B_factor...................... = %f \n",B_fac);
  scanf("%f",&A);
  printf("A_factor...................... = %f \n",A);

  scanf("%d",&contrast);
  if ( contrast == 1 )
    printf("Contrast of final map......... = positive\n");
  else
    printf("Contrast of final map......... = negative\n");
  scanf("%f %f",&phaori[0],&phaori[1]);
    printf("Phase Origin.................. = %f %f\n",phaori[0],phaori[1]);
  scanf("%f %f %f %f",&lattice[0],&lattice[1],&lattice[2],&lattice[3]);
    printf("Reciprocal Lattice............ = %f %f %f %f\n",lattice[0],lattice[1],lattice[2],lattice[3]);


  printf("\n::Starting ML, with noise-flattening on particles\n\n");

  ML(imagename,profilename,resultsfilename);

  printf("\n::2dx_ML finished correctly\n\n");
 
  return 0;
}
