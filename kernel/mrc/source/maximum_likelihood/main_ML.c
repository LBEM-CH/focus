/*
 *
 *  Created by Xiangyan Zeng on 1/1/2007
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */

/*    Execute one time maximum_likelihood.c, the two references used in resolution.c are obtained in maximum_likelihood.c  */




#include <string>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>


#include <common.h>

#include "2dx_lowpass.c"

#include "whitening.c"
#include "ctf.c"
#include "reciprocal.c"
#include "get_units.c"
#include "normalize_image.c"
#include "mask.c" 
#include "trans.c"
#include "quadintp.c"
#include "rotate.c"
#include "cross_corr.c" 
#include "get_resolution.c" 
#include "apply_envelop.c"
#include "rot_refer.c"
#include "Symmetrize.c"


#include "maximum_likelihood.c"
#include "amp_ph.c" 

using namespace std;


int ML(char *filename, char *profile,  char *resultsfilename )
{  


	FILE   *output[5], *results;


	int    i,j, k,m,n;
	float  tmp, *FSC;

	float  *unbend_image1,*unbend_image2;
	float  *peak_x, *peak_y, *peak_z, *real_lat;
	int    all_peak_counter;
	int    sx,sy, num_peaks,num_images ;
	float  *refer,*refer_env,*refer1,*refer2;
	double  max,min,mean,devi; 
	float   temp,r1,r2, x,y,z;
	float *temp_image;
	int length=400;
	float *inter_image;

	FILE  *input;

	mrcImage *image = new mrcImage(filename);
	sx=image->width(); sy=image->height(); 
	printf("sx=%d sy=%d \n",sx,sy);
	fflush(stdout);


	unbend_image1=(float *)calloc(sx*sy,sizeof(float));
	unbend_image2=(float *)calloc(sx*sy,sizeof(float));


	real_lat=(float *)calloc(4,sizeof(float));  
	inter_image=(float *)calloc(length*length,sizeof(float));

	refer=(float *)calloc(realcell_x*realcell_y,sizeof(float)); 
	refer_env=(float *)calloc(realcell_x*realcell_y,sizeof(float)); 
	refer1=(float *)calloc(realcell_x*realcell_y,sizeof(float)); 
	refer2=(float *)calloc(realcell_x*realcell_y,sizeof(float)); 
	temp_image=(float *)calloc(realcell_x*realcell_y,sizeof(float));

	FSC=(float *)calloc(realcell_x,sizeof(int)); 

	results=fopen(resultsfilename,"a");

	output[0]=fopen("reference.pgm","w");
	output[1]=fopen("reference_1.pgm","w");
	output[2]=fopen("reference_2.pgm","w");
	fprintf(output[0],"P2 %d %d \n 256 \n",realcell_y,realcell_x);
	fprintf(output[1],"P2 %d %d \n 256 \n",realcell_y,realcell_x);
	fprintf(output[2],"P2 %d %d \n 256 \n",realcell_y,realcell_x);



	/*    input images  */

	printf("mode is %d \n",image->mode()); 

	srandom(12379);


	for(i=0;i<sx;i++) 
		for(j=0;j<sy;j++) 
		{     
			if(image->mode()==0) temp=(float)((unsigned char*)image->rawData())[j*image->width()+i];
			else if(image->mode()==1)  temp=(float)((unsigned short*)image->rawData())[j*image->width()+i];  
			else if(image->mode()==2) temp=(float)((float*)image->rawData())[j*image->width()+i];
			else {printf("Unsupported mode.\n Exiting..."); exit(1);}

			unbend_image1[IDX(i,j,sx,sy)]=temp;
			unbend_image2[IDX(i,j,sx,sy)]=temp;	 
		}	 


	if(do_whiten==1)
		whitening(sx,sy,unbend_image1);

	if(correct_CTF==1)
		ctfapply(sx,sy,unbend_image2);


	/*     Calculate Unit-cell center in pixels, using phaseorigin and the reciprocal space lattice */

	reciprocal(lattice, real_lat, sx);
  real_lat[0]*=-1;
  real_lat[1]*=-1;
  real_lat[2]*=-1;
  real_lat[3]*=-1;

	x_center = real_lat[0] * (phaori[0]/360.0) + real_lat[2] * phaori[1]/360.0;
	y_center = real_lat[1] * (phaori[0]/360.0) + real_lat[3] * phaori[1]/360.0;

	printf(":\n");
	printf("Phase Origin = %f,%f\n",phaori[0],phaori[1]);
	printf(":Calculated x/y center offset = %f,%f\n",x_center,y_center);
	printf(":\n");
	fflush(stdout);

	/*    input the peaks  */ 

	input=fopen(profile,"r");
	all_peak_counter=0;
	num_peaks=0;

	int nc,nr,ic,ir,mina,maxa,minb,maxb;
	float a1,a2,b1,b2,denmax;

	char *c, oneline[120];
	for(i=0;i<5;i++)
		c=fgets(oneline,120,input);

	fscanf(input,"%d %d %d %d %f %f %f %f %d %d %d %d",&nc,&nr,&ic,&ir,&a1,&a2,&b1,&b2,&mina,&maxa,&minb,&maxb);
	fscanf(input,"%f",&denmax);

	int dimension = (maxa-mina+1)*(maxb-minb+1);

	if(dimension<201)
	{ printf("::ERROR: Not enough peaks in profile !!! (dimension=%d)\n",dimension);
		printf("::MINA=%d, MAXA=%d, MINB=%d, MAXB=%d\n",&mina,&maxa,&minb,&maxb);
		printf("::ABORTING.\n");
		return -1;
	}

	peak_x=(float *)calloc(dimension,sizeof(float));
	peak_y=(float *)calloc(dimension,sizeof(float));
	peak_z=(float *)calloc(dimension,sizeof(float));

	/* Read peaks into core */
	for(i=0;i<dimension;i++)
	{
		fscanf(input,"%f %f %f",&x,&y,&z);
		peak_x[i]=x+x_center;  
		peak_y[i]=y+y_center;  
		peak_z[i]=z;  
	}
	fclose(input);

	/* sort peaks, here a simple bubble sort. Quicksort would be better (ToDo) */
	for(i=0;i<dimension-2;i++)
		for(j=dimension-1;j>i;j--)
			if(peak_z[j]>peak_z[j-1])
			{
				tmp=peak_x[j];
				peak_x[j]=peak_x[j-1];
				peak_x[j-1]=tmp;
				tmp=peak_y[j];
				peak_y[j]=peak_y[j-1];
				peak_y[j-1]=tmp;
				tmp=peak_z[j];
				peak_z[j]=peak_z[j-1];
				peak_z[j-1]=tmp;
			}

	/* count peaks > 0.0 */
	for(i=dimension-1;i>=0;i--)
	{
		if(peak_z[i]>0.0) break;
	}

	all_peak_counter=i;

	if(all_peak_counter<2)
	{ printf("::ERROR: Not enough non-zero peaks in profile !!! (all_peak_counter=%d)\n",all_peak_counter);
		printf("::ABORTING.\n");
		return -1;
	}


	printf(":Total number of peaks ......... = %d\n",all_peak_counter);

	if(threshold_method==0)
	{ /* determine number of peaks above threshold */
		for(i=0;i<all_peak_counter;i++)
			if(peak_z[i]<threshold) break;
		num_peaks=i-1;
	}
	else
	{ /* determined threshold value for inclusion of relative percentage */
		num_peaks=(int)(all_peak_counter*relative_threshold/100.0);
		threshold=peak_z[num_peaks];
	}


	printf(":Threshold ..................... = %f\n",threshold);
	fflush(stdout);
	printf("::Number of peaks above threshold = %d\n",num_peaks);
	fflush(stdout);
	printf(": \n");
	fflush(stdout);

	printf("::Getting particle stack \n"); 
	fflush(stdout);
	printf("<<@progress: 25>>\n");
	fflush(stdout);



	for(i=0;i<realcell_x;i++)
		for(j=0;j<realcell_y;j++)
			refer[IDX(i,j,realcell_x,realcell_y)]=0.0;

	/*  Output stacks of windowed particles into binary file     */  
	output[3]=fopen("stack_whit.binary","w");
	output[4]=fopen("stack_ctf.binary","w"); 

	/*     Get  the  unit cell windows of whitened data */ 
	float *Image1, *Image2;
	Image1=(float *)calloc(500*realcell_x*realcell_y, sizeof(float));
	Image2=(float *)calloc(500*realcell_x*realcell_y, sizeof(float));
	int tag=0;
	m=0;
	while(tag<num_peaks)
	{      
		if(num_peaks-tag<500)
		{	get_units(&num_images,num_peaks-tag,tag,peak_x,peak_y,sx,sy,realcell_x1,realcell_y1,realcell_x,realcell_y,unbend_image1,unbend_image2, Image1,  Image2);      
			tag=num_peaks;
		}
		else
		{	get_units(&num_images,500,tag,peak_x,peak_y,sx,sy,realcell_x1,realcell_y1,realcell_x,realcell_y,unbend_image1, unbend_image2, Image1, Image2);      
			tag+=500;
		}	
		m+=num_images; 

		if(tag<1 && num_images==0)
		{      printf("::ERROR: ML_threshold is too high. No peaks above threshold.\n");
			printf("<<@progress: 30>>\n");
			fflush(stdout);
			return -1;
		}

		normalize_image(num_images, realcell_x,realcell_y,Image1);
		normalize_image(num_images, realcell_x,realcell_y,Image2);  

		for(k=0;k<num_images;k++)  
			for(i=0;i<realcell_x;i++)
				for(j=0;j<realcell_y;j++)
					refer[IDX(i,j,realcell_x,realcell_y)]+=Image2[IDX(i,j,realcell_x,realcell_y)+k*realcell_x*realcell_y];   	


		for(k=0; k<num_images; k++)
		{               
			for(i=0; i<realcell_x; i++)
				for(j=0; j<realcell_y; j++)
					temp_image[IDX(i,j,realcell_x,realcell_y)]=Image1[IDX(i,j,realcell_x,realcell_y)+k*realcell_x*realcell_y];   	           	        
			fwrite(temp_image, sizeof(float)*realcell_x*realcell_y, 1, output[3]);

			for(i=0; i<realcell_x; i++)
				for(j=0; j<realcell_y; j++)
					temp_image[IDX(i,j,realcell_x,realcell_y)]=Image2[IDX(i,j,realcell_x,realcell_y)+k*realcell_x*realcell_y];   	           	        
			fwrite(temp_image, sizeof(float)*realcell_x*realcell_y, 1, output[4]);
		}

	}


	for(i=0;i<realcell_x;i++)
	{  for(j=0;j<realcell_y;j++)
		fprintf(output[0],"%f  ", refer[IDX(i,j,realcell_x,realcell_y)]);  // (int)((refer[IDX(i,j,realcell_x,realcell_y)]-min)/(max-min)*255+1));
		fprintf(output[0],"\n");
	} 
	fclose(output[0]);	


	fclose(output[3]);
	fclose(output[4]);  
	free(unbend_image1);
	free(unbend_image2);

	printf("::main_ML: %d  unit cells are actually used   \n",m); 
	fflush(stdout);


	/*  Initialize the reference  */

	if(ref_ind==1)
		/*     Using random noise as reference    */       
	{   for(i=0;i<realcell_x;i++)
		for(j=0;j<realcell_y;j++)
		{   
			r1=(float)random()/RAND_MAX;
			r2=(float)random()/RAND_MAX;

			refer[IDX(i,j,realcell_x,realcell_y)]=powf(-2*log(r1),0.5)*cos(2*3.1415926*r2);     
		}
	}
	else if(ref_ind==2)
		/*     Using random particle as reference   */   
	{   for(i=0;i<realcell_x;i++)
		for(j=0;j<realcell_y;j++)
			refer[IDX(i,j,realcell_x,realcell_y)]=Image1[IDX(i,j,realcell_x,realcell_y)+10*realcell_x*realcell_y];     
	}   
	else  if(ref_ind!=0 && ref_ind!=1 && ref_ind!=2)
		printf("0:average reference; 1:noise reference; 2:random reference \n"); 


	mask(mask_radius,mask_radius,realcell_x,realcell_y,refer);


	/*    Normalize the reference  */
	normalize_image(1,realcell_x,realcell_y,refer);

	/*    maximum likelihood of the unit cells   */

	printf("<<@progress: 40>>\n");
	printf("::Starting maximum_likelihood calculation\n");
	fflush(stdout);

	maximum_likelihood(realcell_x,realcell_y,refer,refer1,refer2,results);


	/*   Output reference before applying envelope into MRC format file */
	max=refer[0];
	min=max;
	for(i=0;i<realcell_x;i++)
		for(j=0;j<realcell_y;j++)
		{ if(refer[IDX(i,j,realcell_x,realcell_y)]>=max) max=refer[IDX(i,j,realcell_x,realcell_y)];
			else if(refer[IDX(i,j,realcell_x,realcell_y)]<=min) min=refer[IDX(i,j,realcell_x,realcell_y)];
		}

	for(i=0;i<realcell_x;i++)
		for(j=0;j<realcell_y;j++)
			temp_image[IDX(i,j,realcell_x,realcell_y)]=contrast*(refer[IDX(i,j,realcell_x,realcell_y)]-min)*256/(max-min)-(contrast-1.0)*128.0;

	mrcImage::mrcHeader *header = mrcImage::headerFromData(realcell_x,realcell_y,2,(char*)temp_image);

	cout<<"Header Generated"<<endl;
	char fileName1[] = "ML_result_noEnvelope.mrc";
	mrcImage(header,(char*)temp_image,fileName1);
	cout<<fileName1<<" written"<<endl;

	fprintf(results,"# IMAGE-IMPORTANT: %s <ML result without EnvelopeCor.>\n",fileName1);


	printf("<<@progress: 60>>\n");
	fflush(stdout);


	/*    mask  the references */
	// mask(mask_radius,mask_radius,realcell_x,realcell_y,refer);
	mask(mask_radius,mask_radius,realcell_x,realcell_y,refer1);
	mask(mask_radius,mask_radius,realcell_x,realcell_y,refer2);

	/*    get the FSC resolution  */	

	for(i=0;i<realcell_x;i++)
		FSC[i]=0.0;      
	resolution(realcell_x,realcell_y,refer1,refer2,FSC);

	/*   Calculate the SSNR resolution */
	//     spec_snr(num_images,realcell_x,realcell_y,refer,Image2, ssnr);   






	/*  Get amp and phase */ 
	reciprocal(lattice, real_lat, sx);
	amp_ph(realcell_x, realcell_y, length, real_lat, refer, inter_image, results); 
	free(real_lat);




	/*    Apply envelope function to the reference map  */  

	for(i=0;i<realcell_x;i++)
		for(j=0;j<realcell_y;j++)
		        refer_env[IDX(i,j,realcell_x,realcell_y)]=refer[IDX(i,j,realcell_x,realcell_y)];

	envelop(realcell_x,realcell_y,refer_env,A,B_fac,FSC);  


	/*   Output reference after applying envelope into MRC format file */

	max=-1.0e20;
	min=-max;
	for(i=0;i<realcell_x;i++)
		for(j=0;j<realcell_y;j++)
		{ if(refer_env[IDX(i,j,realcell_x,realcell_y)]>=max) max=refer_env[IDX(i,j,realcell_x,realcell_y)];
			else if(refer_env[IDX(i,j,realcell_x,realcell_y)]<=min) min=refer_env[IDX(i,j,realcell_x,realcell_y)];
		}

	for(i=0;i<realcell_x;i++)
		for(j=0;j<realcell_y;j++)
			temp_image[IDX(i,j,realcell_x,realcell_y)]=contrast*(refer_env[IDX(i,j,realcell_x,realcell_y)]-min)*256/(max-min)-(contrast-1.0)*128.0;

	mrcImage::mrcHeader *header2 = mrcImage::headerFromData(realcell_x,realcell_y,2,(char*)temp_image);

	cout<<"Header Generated"<<endl;
	char fileName2[] = "ML_result_withEnvelope.mrc";
	mrcImage(header2,(char*)temp_image,fileName2);
	cout<<fileName2<<" written"<<endl;

	fprintf(results,"# IMAGE-IMPORTANT: %s <ML result with EnvelopeCor.>\n",fileName2);



	/*    Ouput the references    */
	/* 	 	 
				 for(i=0;i<realcell_x;i++)
				 {  for(j=0;j<realcell_y;j++)
				 fprintf(output[0],"%f  ", refer[IDX(i,j,realcell_x,realcell_y)]);  // (int)((refer[IDX(i,j,realcell_x,realcell_y)]-min)/(max-min)*255+1));
				 fprintf(output[0],"\n");
				 } 
				 fclose(output[0]);
	 */     


	max=-1e20;min=1e20;	 
	for(i=0;i<realcell_x;i++)
		for(j=0;j<realcell_y;j++)
			if(refer1[IDX(i,j,realcell_x,realcell_y)]>=max) max=refer1[IDX(i,j,realcell_x,realcell_y)];
			else if(refer1[IDX(i,j,realcell_x,realcell_y)]<=min) min=refer1[IDX(i,j,realcell_x,realcell_y)];


	for(i=0;i<realcell_x;i++)
	{  for(j=0;j<realcell_y;j++)
		fprintf(output[1],"%d  ", (int)((refer1[IDX(i,j,realcell_x,realcell_y)]-min)/(max-min)*255+1));
		fprintf(output[1],"\n");
	} 
	fclose(output[1]);


	for(i=0;i<realcell_x;i++)
		for(j=0;j<realcell_y;j++)
			temp_image[IDX(i,j,realcell_x,realcell_y)]=contrast*(refer1[IDX(i,j,realcell_x,realcell_y)]-min)*256/(max-min)-(contrast-1.0)*128.0;

	mrcImage::mrcHeader *header3 = mrcImage::headerFromData(realcell_x,realcell_y,2,(char*)temp_image);
	char fileName3[] = "ML_result_ref_even.mrc";
	mrcImage(header3,(char*)temp_image,fileName3);
	cout<<fileName3<<" written"<<endl;

	fprintf(results,"# IMAGE-IMPORTANT: %s <ML result, Even Reference (no envelope)>\n",fileName3);

	max=-1e20;min=1e20;	 
	for(i=0;i<realcell_x;i++)
		for(j=0;j<realcell_y;j++)
			if(refer2[IDX(i,j,realcell_x,realcell_y)]>=max) max=refer2[IDX(i,j,realcell_x,realcell_y)];
			else if(refer2[IDX(i,j,realcell_x,realcell_y)]<=min) min=refer2[IDX(i,j,realcell_x,realcell_y)];


	for(i=0;i<realcell_x;i++)
	{  for(j=0;j<realcell_y;j++)
		fprintf(output[2],"%d  ", (int)((refer2[IDX(i,j,realcell_x,realcell_y)]-min)/(max-min)*255+1));
		fprintf(output[2],"\n");
	} 
	fclose(output[2]);

	for(i=0;i<realcell_x;i++)
		for(j=0;j<realcell_y;j++)
			temp_image[IDX(i,j,realcell_x,realcell_y)]=contrast*(refer2[IDX(i,j,realcell_x,realcell_y)]-min)*256/(max-min)-(contrast-1.0)*128.0;

	mrcImage::mrcHeader *header4 = mrcImage::headerFromData(realcell_x,realcell_y,2,(char*)temp_image);
	char fileName4[] = "ML_result_ref_odd.mrc";
	mrcImage(header4,(char*)temp_image,fileName4);
	cout<<fileName4<<" written"<<endl;

	fprintf(results,"# IMAGE-IMPORTANT: %s <ML result, Odd Reference (no envelope)>\n",fileName4);

	printf("<<@progress: 70>>\n");
	fflush(stdout);



	max=-1e20;min=1e20;	 
	for(i=0;i<length;i++)
		for(j=0;j<length;j++)
			if(inter_image[IDX(i,j,length,length)]>=max) max=inter_image[IDX(i,j,length,length)];
			else if(inter_image[IDX(i,j,length,length)]<=min) min=inter_image[IDX(i,j,length,length)];

	for(i=0;i<length;i++)
		for(j=0;j<length;j++)
			inter_image[IDX(i,j,length,length)]=contrast*(inter_image[IDX(i,j,length,length)]-min)*256/(max-min)-(contrast-1.0)*128.0;

	mrcImage::mrcHeader *header5 = mrcImage::headerFromData(length,length,2,(char*)inter_image);

	cout<<"Header Generated"<<endl;
	char fileName5[] = "ML_Interpolated.mrc";
	mrcImage(header5,(char*)inter_image,fileName5);
	cout<<fileName5<<" written"<<endl;

	fprintf(results,"# IMAGE-IMPORTANT: %s <ML result UnitCell (no envelope)>\n",fileName5); 


	fclose(results);

	free(refer);
	free(refer_env);
	free(refer1);
	free(refer2);
	free(inter_image);
	free(FSC);
	free(temp_image);



}  






