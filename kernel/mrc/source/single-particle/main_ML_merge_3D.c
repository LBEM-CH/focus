/*
 *
 *  Created by Xiangyan Zeng on 1/1/2007
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 *    Main program of merging mltiple crystals using maximum likelihood  
 */
#include <string>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define IRADA 0
#define IPAD  3
#define IRAD 1

 
int  max_ang1=5, max_ang2=5, max_ang3=5;
int  step_angle1=1,step_angle2=1, step_angle3=1;
 

 

int sx,sy;
float  CS, KV, RESMIN, RESMAX, DSTEP, XMAG;

typedef struct
{  
    fftwf_plan  dft2_fw;
    fftwf_plan  dft2_bw;
    float *ref; 
    int dim;
    int rmax1;
    int rmax2;
    int shift;
    int num_angles;
    int start;
    int end;
    int numLoop;
    float *pdf;
    float dev_x;
    float dev_y;
    float dev_phi;
    float dev_theta;
    float dev_psi;
    float *re_reference;
    float *im_reference;
    float *pow_reference;
    float *re_samp;
    float *im_samp;
    float *image;
    float *re_samp_CTF;
    float *im_samp_CTF;
    float *pow_samp;
    float *angle;
    float *sigma;
    float *new_re_refer;
    float *new_im_refer;
    float *new_re_refer1;
    float *new_im_refer1;
    float *new_re_refer2;
    float *new_im_refer2;

    float *new_re_refer_CTF;
    float *new_im_refer_CTF;
    float *normal;
    float *normal1;
    float *normal2;
	int *count;
	int *count1;
	int *count2;

    float *normal_CTF;
    float *new_par;
}my_struct;






#include "2dx_lowpass.c"
#include "mask2D.c"

#include "mask_refer.c"
#include "mask3D.c" 
#include "trans3D.c"
#include "trans.c"
#include "quadintp.c"
#include "rotate.c"
#include "flip.c"
#include "cross_corr.c" 
//#include "get_resolution_3D.c" 
#include "apply_envelop.c"
#include "align.c"
#include "rot_refer_merge.c"
#include "Symmetrize.c"
#include "Symmetrize3D.c"
#include "align_refer.c"
#include "box_ft.c"
#include "ainterpo3dbig.c"
#include "ainterpo3d.c"
#include "insert3D.c"
#include "extract2D.c"
#include "maximum_likelihood_merge_3D_cp.c"
#include "reciprocal.c"
#include "amp_ph.c" 
 

// maximum_likelihood_merge_3D_cp.c handle each stack independently, while maximum_likelihood_merge_3d.c handle all the stack together


using namespace std;

 
int ML(int Numdirs, char *dirfilename,  char *resultsfilename )
{  
 
       
	FILE   *output[3], *results;      

	int    i,j, k,m,n;
        float  tmp, *FSC;
	float  *refer,*refer1,*refer2;
	double  max,min,mean,devi; 
	float   temp,r1,r2;
	float *temp_image;
     int length=100;
     float *real_lat, *inter_image;
    
	inter_image=(float *)malloc(sizeof(float)*length*length);
 
	refer=(float *)malloc(sizeof(float)*realcell_x*realcell_y*realcell_y); 
	refer1=(float *)malloc(sizeof(float)*realcell_x*realcell_y*realcell_y); 
	refer2=(float *)malloc(sizeof(float)*realcell_x*realcell_y*realcell_y); 
	temp_image=(float *)malloc(sizeof(float)*realcell_x*realcell_y*realcell_y);  
 	FSC=(float *)malloc(sizeof(int)*realcell_x); 
       
	results=fopen(resultsfilename,"a");
    
	output[0]=fopen("reference.pgm","w");
     output[1]=fopen("reference_1.pgm","w");
     output[2]=fopen("reference_2.pgm","w");
     fprintf(output[0],"P2 %d %d \n 256 \n",realcell_y,realcell_x);
     fprintf(output[1],"P2 %d %d \n 256 \n",realcell_y,realcell_x);
     fprintf(output[2],"P2 %d %d \n 256 \n",realcell_y,realcell_x);
   
	/* Input the image size and lattice of the first crystal for calculating unit cell map   

        FILE *lat;
        float *lattice;
        lattice=(float *)calloc(4,sizeof(float));
               
        lat=fopen("lattice.dat","r");
        fscanf(lat,"%d %d ",&sx,&sy);
        fscanf(lat,"%f  %f  %f  %f  %f  %f  ", &DSTEP, &XMAG,&CS, &KV,  &RESMAX, &RESMIN);
//        printf("%f  %f  %f  %f  %f  %f  ", DSTEP, XMAG,CS, KV,  RESMAX, RESMIN);         
        fscanf(lat, "%f %f %f %f",&lattice[0],&lattice[1],&lattice[2],&lattice[3]);
//        printf("%f %f %f %f",lattice[0],lattice[1],lattice[2],lattice[3]);
        fclose(lat);
	*/


 
  
/*  ToDo: Output stacks of whitened or CTF corrected particles into MRC format file */
  	
/*  Initialize the reference  */
 	 
	  /*     Using random noise as reference    */       
	for(i=0;i<realcell_x;i++)	
	  for(j=0;j<realcell_y;j++)
	    for(k=0;k<realcell_y;k++)
	    {    
			r1=(float)random()/RAND_MAX;
	       		r2=(float)random()/RAND_MAX;   	   
	        	refer[k+j*realcell_x+i*realcell_x*realcell_x]=(1.0*powf(-2*log(r1),0.5)*cos(2*3.1415926*r2));
	    }
 
	// mask3D(mask_radius,mask_radius,realcell_x,realcell_y,refer);
       
	
	/*    Normalize the reference   
	normalize_image(1,realcell_x,realcell_y,refer);
        */

    
	/*    maximum likelihood of the unit cells   */
    
	printf("<<@progress: 40>>\n");
        printf("::Starting maximum_likelihood \n\n");
 	maximum_likelihood(Numdirs,realcell_x,realcell_y,dirfilename,refer,refer1,refer2,results);

  
/*
	for(i=0;i<realcell_x;i++)
            FSC[i]=0.0;      
	resolution3D(realcell_x,realcell_y,refer1,refer2,FSC, DSTEP, XMAG);
*/



	/*   Output reference before applying envelope into MRC format file  
 
	max=-1.0e20;
        min=-max;
        for(i=0;i<realcell_x;i++)
        	for(j=0;j<realcell_y;j++)
        		{   if(refer[j+i*realcell_y]>=max) max=refer[j+i*realcell_y];
                            else if(refer[j+i*realcell_y]<=min) min=refer[j+i*realcell_y];
                        }

	for(i=0;i<realcell_x;i++)
        	for(j=0;j<realcell_y;j++)
               		temp_image[j+i*realcell_y]=contrast*(refer[j+i*realcell_y]-min)*256/(max-min)-(contrast-1.0)*128.0;
 
	mrcImage::mrcHeader *header = mrcImage::headerFromData(realcell_x,realcell_y,2,(char*)temp_image);
   
	cout<<"Header Generated"<<endl;
        char fileName1[] = "ML_result_noEnvelope.mrc";
        mrcImage(header,(char*)temp_image,fileName1);
        cout<<fileName1<<" written"<<endl;
	fprintf(results,"# IMAGE-IMPORTANT: %s <ML result without EnvelopeCor.>\n",fileName1);
 
 	printf("<<@progress: 60>>\n");
        fflush(stdout);
        */
 	

	/*    mask  the references */
//	mask3D(mask_radius,mask_radius,realcell_x,realcell_y,refer);
//        mask3D(mask_radius,mask_radius,realcell_x,realcell_y,refer1);
//        mask3D(mask_radius,mask_radius,realcell_x,realcell_y,refer2);

	/*    get the FSC resolution   	
    
	for(i=0;i<realcell_x;i++)
            FSC[i]=0.0;      
	resolution(realcell_x,realcell_y,refer1,refer2,FSC);
  */


	/*    Apply envelope function to the reference map     

       envelop(realcell_x,realcell_y,refer,A,B_fac,FSC);  
 */


	/*  Get amp and phase  
     
	reciprocal(lattice, real_lat, sx);
        amp_ph(realcell_x, realcell_y, length, real_lat, refer, inter_image, results); 
        free(lattice);
 */



	/*   Output reference after applying envelope into MRC format file  
 
   	max=-1.0e20;
        min=-max;
        for(i=0;i<realcell_x;i++)
        	for(j=0;j<realcell_y;j++)
         		{    if(refer[j+i*realcell_y]>=max) max=refer[j+i*realcell_y];
           		     else if(refer[j+i*realcell_y]<=min) min=refer[j+i*realcell_y];
                        }

	for(i=0;i<realcell_x;i++)
        	for(j=0;j<realcell_y;j++)
               		temp_image[j+i*realcell_y]=contrast*(refer[j+i*realcell_y]-min)*256/(max-min)-(contrast-1.0)*128.0;
 
	mrcImage::mrcHeader *header2 = mrcImage::headerFromData(realcell_x,realcell_y,2,(char*)temp_image);
     
	cout<<"Header Generated"<<endl;
	char fileName2[] = "ML_result_withEnvelope.mrc";
	mrcImage(header2,(char*)temp_image,fileName2);
	cout<<fileName2<<" written"<<endl;

         fprintf(results,"# IMAGE-IMPORTANT: %s <ML result with EnvelopeCor.>\n",fileName2);
      */
	 

	/*    Ouput the references     
		 	 
	for(i=0;i<realcell_x;i++)
           {    for(j=0;j<realcell_y;j++)
	              fprintf(output[0],"%d  ", (int)((refer[j+i*realcell_y]-min)/(max-min)*255+1));
       	         fprintf(output[0],"\n");
          } 
	fclose(output[0]);
    
	max=-1e20;min=1e20;	 
      	for(i=0;i<realcell_x;i++)
       		for(j=0;j<realcell_y;j++)
	  		if(refer1[j+i*realcell_y]>=max) max=refer1[j+i*realcell_y];
	  		else if(refer1[j+i*realcell_y]<=min) min=refer1[j+i*realcell_y];		 
		 	 
	for(i=0;i<realcell_x;i++)
         {  for(j=0;j<realcell_y;j++)
	         fprintf(output[1],"%d  ", (int)((refer1[j+i*realcell_y]-min)/(max-min)*255+1));
             fprintf(output[1],"\n");
	 } 
	fclose(output[1]);
      
	
	for(i=0;i<realcell_x;i++)
        	for(j=0;j<realcell_y;j++)
        	       temp_image[j+i*realcell_y]=contrast*(refer1[j+i*realcell_y]-min)*256/(max-min)-(contrast-1.0)*128.0;
 
	mrcImage::mrcHeader *header3 = mrcImage::headerFromData(realcell_x,realcell_y,2,(char*)temp_image);
	char fileName3[] = "ML_result_ref_even.mrc";
	mrcImage(header3,(char*)temp_image,fileName3);
	cout<<fileName3<<" written"<<endl;

	fprintf(results,"# IMAGE-IMPORTANT: %s <ML result, Even Reference>\n",fileName3);http://www.forsalebyowner.com/search?szLocation=warner+robins%2C+ga&x=32&y=11&iPage=4
      
	max=-1e20;min=1e20;	 
	for(i=0;i<realcell_x;i++)
	       for(j=0;j<realcell_y;j++)
         	     if(refer2[j+i*realcell_y]>=max) max=refer2[j+i*realcell_y];
                    else if(refer2[j+i*realcell_y]<=min) min=refer2[j+i*realcell_y];
		 
		 	 
	for(i=0;i<realcell_x;i++)
            {    for(j=0;j<realcell_y;j++)
	              fprintf(output[2],"%d  ", (int)((refer2[j+i*realcell_y]-min)/(max-min)*255+1));
                  fprintf(output[2],"\n");
	    } 
	fclose(output[2]);
   	

	for(i=0;i<realcell_x;i++)
        	for(j=0;j<realcell_y;j++)
               		temp_image[j+i*realcell_y]=contrast*(refer2[j+i*realcell_y]-min)*256/(max-min)-(contrast-1.0)*128.0;
 
	mrcImage::mrcHeader *header4 = mrcImage::headerFromData(realcell_x,realcell_y,2,(char*)temp_image);
	char fileName4[] = "ML_result_ref_odd.mrc";
	mrcImage(header4,(char*)temp_image,fileName4);
        cout<<fileName4<<" written"<<endl;
        fprintf(results,"# IMAGE-IMPORTANT: %s <ML result, Odd Reference>\n",fileName4);	
        printf("<<@progress: 70>>\n");
        fflush(stdout);


	max=-1e20;min=1e20;	 
	for(i=0;i<length;i++)
        	for(j=0;j<length;j++)
	  		if(inter_image[j+i*length]>=max) max=inter_image[j+i*length];
	                else if(inter_image[j+i*length]<=min) min=inter_image[j+i*length];
      
	for(i=0;i<length;i++)
        	for(j=0;j<length;j++)
           		inter_image[j+i*length]=contrast*(inter_image[j+i*length]-min)*256/(max-min)-(contrast-1.0)*128.0;
 
	mrcImage::mrcHeader *header5 = mrcImage::headerFromData(length,length,2,(char*)inter_image);
   
	cout<<"Header Generated"<<endl;
        char fileName5[] = "ML_Interpolated.mrc";
        mrcImage(header5,(char*)inter_image,fileName5);
        cout<<fileName5<<" written"<<endl;

        fprintf(results,"# IMAGE-IMPORTANT: %s <ML result UnitCell>\n",fileName5); 
 
 
        fclose(results);
        */



	free(refer);
	free(refer1);
	free(refer2);
	free(inter_image);
	free(FSC);
 
	 
}  



 


