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
float  B_fac,A,lattice[4],phaori[2], defocus[3];
int    mask_radius,Iteration, ref_ind, correct_CTF,Symmetry,threshold_method,lp_method;
int    realcell_x1, realcell_y1, realcell_x, realcell_y, oversized_x,oversized_y,do_whiten, DS_ratio;
int    doMLorCC,contrast;



#include "string_to_real.c"
#include "string_to_integer.c"
#include "cgetline.c"
#include "fgetline.c"
#include "igetline.c"
#include "main_ML.c"            /*  ML using whitening of the whole image (Niko's algorithm)  */  
// #include "main_ML_particle.c"     /*  ML using whitening of individual particle */


int main()
{ 
	int dir,m,i,j,k,n,num_line, Symmetry1;
	int num_images,  *pa,sx;   /*  Prepare for merge Nimages images together into a stack of num_images patches*/

	FILE *input;
	char oneline[100];
	char *c; 
        char *tmpstring;
	char  *imagename, *profilename, *resultsfilename, *imagenumber, *temp_imagename;
	int oversizexy[2], realcellxy[2], realcellxy1[2];
	float angle[3];


        tmpstring=(char *)calloc(200,sizeof(char));
	imagename=(char *)calloc(400,sizeof(char));
	profilename=(char *)calloc(400,sizeof(char));
	imagenumber=(char *)calloc(200,sizeof(char));
	temp_imagename=(char *)calloc(200,sizeof(char));
	resultsfilename=(char *)calloc(200,sizeof(char));

	printf("\n2dx_ML: Performs Maximum Likelihood processing. \n");

	scanf("%d",&doMLorCC);
	if(doMLorCC==0)
		printf("Weights for ref calculation... = from Maximum Likelihood estimate\n");
	else
		printf("Weights for ref calculation... = from Cross-Correlation between particle and old reference\n");

	scanf("%s",resultsfilename);
	printf("Results File Name............. = %s \n",resultsfilename);

	input=fopen("2dx_image.cfg" ,"r"); 

	/*  input  parameters from 2dx_image.cfg to generate stacks*/

	printf("\nReading  Parameters for ML:\n");
        strcpy(tmpstring,"imagename");
	imagename=cgetline(input,tmpstring);
	strcpy(temp_imagename, imagename);
	strcat(imagename,".mrc");
	printf("Image Name..................... = %s \n",imagename);

        strcpy(tmpstring,"imagenumber");
	imagenumber=cgetline(input,tmpstring);
	printf("Image Number................... = %s \n",imagenumber);

	//strcpy(profilename,"SCRATCH/prof");
	//strcat(profilename,temp_imagename);
	//strcat(profilename,".dat");
	//  strcpy(profilename,temp_imagename);
	//   strcat(profilename,"-profile.dat");
	strcpy(profilename,temp_imagename);
	strcat(profilename,"-profile.dat");
	printf("Profile Name................... = %s \n",profilename);


        strcpy(tmpstring,"CS");
	if(fgetline(input,tmpstring,&CS)==0); 
	printf("CS............................. = %f  \n",CS); 

        strcpy(tmpstring,"KV");
	if(fgetline(input,tmpstring, &KV )==0)
		printf("KV............................. = %f  \n",KV); 

        strcpy(tmpstring,"RESMIN");
	if(fgetline(input,tmpstring, &RESMIN )==0)  
		printf("RESMIN......................... = %f  \n",RESMIN); 

        strcpy(tmpstring,"RESMAX");
	if(fgetline(input,tmpstring,&RESMAX )==0); 
	printf("RESMAX......................... = %f  \n",RESMAX); 

        strcpy(tmpstring,"defocus");
	if(fgetline(input,tmpstring, defocus)==0)
	{ 
		DIFMID1=defocus[0];
		DIFMID2=defocus[1];
		ANGAST=defocus[2];
		printf("DFMID1,DFMID2,ANGAST........... = %f,%f,%f\n",DIFMID1,DIFMID2, ANGAST); 
	}

        strcpy(tmpstring,"ML_do_whiten");         
	if(*(cgetline(input,tmpstring))=='y')
	{   do_whiten =1;
		printf("do_whiten...................... = yes \n");
	}
	else
	{   do_whiten=0;
		printf("do_whiten...................... = no \n");
	}

        strcpy(tmpstring,"ML_correct_CTF");         
	if(*(cgetline(input,tmpstring))=='y')
	{   correct_CTF =1;
		printf("correct_CTF.................... = yes \n");
	}
	else
	{   correct_CTF=0;
		printf("correct_CTF.................... = no \n");
	}

        strcpy(tmpstring,"stepdigitizer");         
	if(fgetline(input,tmpstring,&DSTEP )==0)  
		printf("DSTEP.......................... = %f\n",DSTEP); 

        strcpy(tmpstring,"magnification");         
	if(fgetline(input,tmpstring, &XMAG)==0)
		printf("Magnification (XMAG)........... = %f \n",XMAG);
	/*
		 if(igetline(input, "ML_oversizexy", oversizexy)==0)  
		 {   oversized_x=oversizexy[0];
		 oversized_y=oversizexy[1];
		 printf("ML_oversizexy................. = %d,%d\n",oversized_x,oversized_y);
		 } 
		 else printf("parameter ML_oversizexy does not exists \n");   
	 */

        strcpy(tmpstring,"ML_realcellxy_outer");         
	if(igetline(input,tmpstring, realcellxy)==0)  
	{   realcell_x=realcellxy[0];
		realcell_y=realcellxy[1];
		printf("ML_realcellxy_outer............ = %d,%d\n",realcell_x,realcell_y);
	}
	else printf("parameter ML_realcellxy_outer does not exists \n");    
	/*    
				if(igetline(input, "ML_realcellxy_inner", realcellxy1)==0)  
				{   realcell_x1=realcellxy1[0];
				realcell_y1=realcellxy1[1];
				printf("ML_realcellxy_inner............ = %d,%d\n",realcell_x1,realcell_y1);
				} 
				else printf("parameter ML_realcell_xy1 does not exists \n");   
	 */
	realcell_x1=realcell_x;
	realcell_y1=realcell_y;

        strcpy(tmpstring,"ML_mask_radius");         
	if(igetline(input,tmpstring, &mask_radius)==0) 
		printf("Mask radius.................... = %d \n",mask_radius); 
	else printf("parameter ML_mask_radius does not exists \n");   

        strcpy(tmpstring,"ML_iteration");         
	if(igetline(input,tmpstring, &Iteration)==0) 
		printf("Iteration...................... = %d \n",Iteration); 
	else printf("parameter ML_iteration does not exists \n");   

        strcpy(tmpstring,"ML_threshold_method");         
	if(igetline(input,tmpstring, &threshold_method)==0) 
	{     
		if(threshold_method==0)
		{   
                        strcpy(tmpstring,"ML_absolute_threshold");      
			if(fgetline(input,tmpstring, &threshold)==0) 
				printf("Absolute Threshold value....... = %f \n",threshold);
			else  printf("parameter ML_absolute_threshold does not exists \n");     
		}
		else  
		{               
                        strcpy(tmpstring,"ML_relative_threshold");      
                        if(fgetline(input,tmpstring, &relative_threshold)==0)    
			printf("Percentage of peaks to use..... = %.2f% \n",relative_threshold);
			else printf("parameter ML_relative_threshold does not exists \n");
		}
	}
	else printf("parameter ML_threshold_method does not exists \n");   

        strcpy(tmpstring,"ML_ref_ind");         
	if(igetline(input,tmpstring, &ref_ind)==0) 
	{    
		if ( ref_ind == 0 ) 
			printf("Initial Reference (ref_ind).... = average reference \n");
		else if ( ref_ind == 1 ) 
			printf("Initial Reference (ref_ind).... = random noise \n");
		else if ( ref_ind == 2 )
			printf("Initial Reference (ref_ind).... = random particle \n");
		else
			printf("::Initial Reference (ref_ind).... = ILLEGAL CHOICE !!!! \n");
	}
	else printf("parameter ML_ref_ind does not exists \n");   

	/*
		 if(igetline(input, "ML_DS_ratio", &DS_ratio)==0) 
		 printf("DS_ratio............................ = %d \n",DS_ratio); 
		 else printf("parameter DS_ratio does not exists \n");  
	 */
        strcpy(tmpstring,"ML_rotational_symmetry");      
	if(igetline(input,tmpstring, &Symmetry1)==0) 
	{   if(Symmetry1==0) Symmetry=1;
		else if(Symmetry1==1) Symmetry=2;
		else if(Symmetry1==2) Symmetry=3;
		else if(Symmetry1==3) Symmetry=4;
		else if(Symmetry1==4) Symmetry=6;
		printf("Symmetry....................... = %d \n",Symmetry); 
	}
	else printf("parameter Symmetry does not exists \n");  

        strcpy(tmpstring,"ML_lp_method");      
	if(igetline(input,tmpstring, &lp_method)==0) 
	{   
		if(lp_method==1)
			printf("Low-Pass filter................ = Gaussian \n");
		else if(lp_method==2)
			printf("Low-Pass filter................ = Sharp cutoff \n");
		else
			printf("Low-Pass filter................ = None \n");
	}
	else printf("parameter ML_lp_method does not exists \n");   	

        strcpy(tmpstring,"ML_lp_radius");      
	if(fgetline(input,tmpstring,&lp_radius)==0)
		printf("lp_radius...................... = %f \n",lp_radius); 
	else printf("paramter lp_radius does not exists \n");

        strcpy(tmpstring,"ML_MinMaxStep_angle");      
	if(fgetline(input,tmpstring, angle)==0)
	{ 
		min_angle=angle[0];
		max_angle=angle[1];
		step_angle=angle[2];
		printf("min_angle,max_angle,step_angle. = %f,%f,%f \n",min_angle, max_angle, step_angle);
	}
	else printf("Angle parameters do not exists \n"); 

        strcpy(tmpstring,"ML_terminate_ML");      
	if(fgetline(input,tmpstring,&Terminate_ML)==0)
		printf("Terminate_ML................... = %f \n",Terminate_ML); 
	else printf("paramter ML_terminate does not exists \n");

        strcpy(tmpstring,"ML_B_factor");      
	if(fgetline(input,tmpstring,&B_fac)==0)
		printf("B_factor....................... = %f \n",B_fac); 
	else printf("paramter B_factor does not exists \n");

        strcpy(tmpstring,"ML_A_factor");      
	if(fgetline(input,tmpstring,&A)==0)
		printf("A_factor....................... = %f \n",A); 
	else printf("paramter A_factor does not exists \n");

        strcpy(tmpstring,"ctfrev");         
	if(*(cgetline(input,tmpstring))=='y')
	{    contrast=-1;
		printf("Contrast of final map.......... = positive\n");
	}
	else
	{   contrast=1;
		printf("Contrast of final map.......... = negative\n");
	}

        strcpy(tmpstring,"phaori");      
	if(fgetline(input,tmpstring,phaori)==0);
	printf("Phase Origin................... = %f %f\n",phaori[0],phaori[1]);

        strcpy(tmpstring,"lattice");      
	if(fgetline(input,tmpstring,lattice)==0)
		printf("Reciprocal Lattice............. = %f %f %f %f\n",lattice[0],lattice[1],lattice[2],lattice[3]);

	fclose(input);


	free(imagenumber); 
	free(temp_imagename); 

	printf("\n::Starting ML\n\n");
	fflush(stdout);

	/*   ML main program */

	ML(imagename, profilename,resultsfilename); 
	printf("\n::2dx_ML finished correctly\n\n");
	fflush(stdout);

	free(profilename); 
	free(imagename); 
	free(resultsfilename);  




	return 0;
}
