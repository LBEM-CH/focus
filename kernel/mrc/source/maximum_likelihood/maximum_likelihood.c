/*  This program obtains the reference using maximum likelihood algorithm 

 *
 *  Created by Xiangyan Zeng on 1/1/2007
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *


It considers rotation and translation and can be used for single particle alignment and 2d crystals
The rotation is assumed to have Gaussian distribution 
The translation offset is assumed to have Gaussian distribution 


 A low-pass filter may be applied to the reference,


Input
   nx, ny:  image patch size
   num_images: number of image patches
   Iteration: maximum iterations of maximum likelihood (normally the stop criterion is that the convergence of parameter is small enough)
   Image: Stack of whitened image patches 
   Image_CTF: Stack of CTF corrected image patches   
   
Output
   refer:                reference obtained from the whole dataset
   refer1:               reference obtained from even numbers
   refer2:               reference obtained from odd numbers 
   Image_refer:
   Image_refer_CTF:
 

*/


#include <string>
#include <stdio.h>
#include <stdlib.h>
#include <mrcImage.h>
#include <common.h>

#ifndef pi
#define pi 3.141592654
#endif
#define Max_num 1000            /*  Maximum number of particles processed at one time */
using namespace std;


void maximum_likelihood( int nx, int ny, float *refer, float *refer1, float *refer2, FILE *results)

{
	FILE  *input[0],  *output[2];

	int CTF=1;


	float Gau=2.0;

	int i,j,k,m,n,k1,k2,k3,k4,Loop,num_refer,Num_angles, num_temp, Num_images;
	double dev_sigma, dev_x,dev_y,sigma, dev_theta, dev_sigma_theta, dev_sigma_theta_change;
	double new_dev_sigma, new_dev_x, new_dev_y, new_dev_theta, new_sigma, new_sigma_theta, dev_sigma_change;

	float *new_refer,*temp_image,*weight,*f,*corr_image,*corr_gama,*Image_refer,*Image_refer_CTF;
	float *angle, *temp_refer,*temp_refer1,*temp_refer2, *final_refer, *final_refer1, *final_refer2, *Image, *Image_CTF, *temp_image1,*temp_image2;
	double angle1,max,min,gama,corr,mean, pow_refer,pow_RT, likelihood;
	double st1, st2, st3, st4, st5,st6,st7;
	float pow_image,*RTrefer,*RTimage, *indx, *indy;
	float *rota;



	Num_angles=(int)((max_angle-min_angle)/step_angle)+1;

	i=0;

	int it = sizeof(float)*nx*ny;

	Image=(float *)calloc(Max_num*nx*ny,sizeof(float));
	Image_CTF=(float *)calloc(Max_num*nx*ny,sizeof(float));
	temp_image1=(float *)calloc(nx*ny,sizeof(float));
	temp_image2=(float *)calloc(nx*ny,sizeof(float));

	indx=(float *)calloc(nx*ny,sizeof(float));
	indy=(float *)calloc(nx*ny,sizeof(float));

	final_refer=(float *)calloc(nx*ny*Num_angles*Iteration,sizeof(float));
	final_refer1=(float *)calloc(nx*ny*Num_angles*Iteration,sizeof(float));
	final_refer2=(float *)calloc(nx*ny*Num_angles*Iteration,sizeof(float));


	temp_image=(float *)calloc(nx*ny,sizeof(float));
	temp_refer=(float *)calloc(nx*ny,sizeof(float));
	temp_refer1=(float *)calloc(nx*ny,sizeof(float));
	temp_refer2=(float *)calloc(nx*ny,sizeof(float));
	RTimage=(float *)calloc(nx*ny,sizeof(float));


	RTrefer=(float *)calloc(nx*ny*Num_angles,sizeof(float));
	weight=(float *)calloc(nx*ny*Num_angles,sizeof(float));
	new_refer=(float *)calloc(nx*ny*Num_angles,sizeof(float));
	// Image_particle=(float *)malloc(sizeof(float)*nx*ny*Num_images);  


	Image_refer=(float *)calloc(nx*ny*Iteration,sizeof(float));
	Image_refer_CTF=(float *)calloc(nx*ny*Iteration,sizeof(float));  


	corr_image=(float *)calloc(nx*ny*Num_angles,sizeof(float));
	corr_gama=(float *)calloc(nx*ny*Num_angles,sizeof(float));

	f=(float *)calloc(nx*ny*Num_angles,sizeof(float));

	angle=(float *)calloc(Num_angles,sizeof(float));

	printf("Allocation done.\n");


	output[0]=fopen("./SCRATCH/output1_ML_whitening.pgm","w");
	output[1]=fopen("./SCRATCH/output2_ML_CTFcorrect.pgm","w");

	fprintf(output[0],"P2 %d %d \n", 5*(ny+2), Iteration/5*(nx+2));
	fprintf(output[0],"256\n");

	fprintf(output[1],"P2 %d %d \n", 5*(ny+2), Iteration/5*(nx+2));
	fprintf(output[1],"256\n");     

	printf(": \n");
	printf("::  Loop   NewSig     Dev_X      Dev_Y      Dev_Sigma    Dev_Theta Dev_Sigma_Theta\n");
	printf("::--------------------------------------------------------------------------------------------\n");
	fflush(stdout);

	Loop=0; 
	mrcImage::mrcHeader *header2 = mrcImage::headerFromData(nx,ny,2,(char*)refer);                
	char outputfile [50];
	sprintf(outputfile,"./ML/ML_reference_%03d.mrc",Loop);           
	mrcImage(header2,(char*)refer,outputfile);
	printf("File %s written.\n",outputfile);               
	fprintf(results,"# IMAGE: %s <Reference Map %03d>\n",outputfile,Loop);
	fflush(results);


	/*   Normalize the input Image   */       

	pow_image=0.0;
	for(k3=0;k3<nx;k3++)
		for(k4=0;k4<ny;k4++)
			pow_image+=powf(refer[IDX(k3,k4,nx,ny)],2.0);


	for(i=0;i<Num_angles;i++)
		angle[i]=min_angle+i*step_angle;  

	/*   Preallocate the array position used for caculating the translation probability   */

	for(i=0;i<nx/2;i++)
		for(j=0;j<ny;j++)
			indx[IDX(i,j,nx,ny)]=i;

	for(i=nx/2;i<nx;i++)
		for(j=0;j<ny;j++)
			indx[IDX(i,j,nx,ny)]=-(nx-i);    

	for(i=0;i<nx;i++)
		for(j=0;j<ny/2;j++)
			indy[IDX(i,j,nx,ny)]=j;

	for(i=0;i<nx;i++)
		for(j=ny/2;j<ny;j++)
			indy[IDX(i,j,nx,ny)]=-(ny-j);




	/*   Initialize the images  */

	for(i=0;i<nx;i++)
		for(j=0;j<ny;j++)
			Image_refer[IDX(i,j,nx,ny)]=refer[IDX(i,j,nx,ny)];

	for(i=0;i<nx;i++)
		for(j=0;j<ny;j++)
			Image_refer_CTF[IDX(i,j,nx,ny)]=refer[IDX(i,j,nx,ny)];	

	num_refer=1;
	for(m=0;m<Iteration;m++)
		for(k=0;k<Num_angles;k++)	      
			for(i=0;i<nx;i++)
				for(j=0;j<ny;j++)
				{   final_refer[IDX(i,j,nx,ny)+k*nx*ny+m*nx*ny*Num_angles]=0.0; 
					final_refer1[IDX(i,j,nx,ny)+k*nx*ny+m*nx*ny*Num_angles]=0.0;
					final_refer2[IDX(i,j,nx,ny)+k*nx*ny+m*nx*ny*Num_angles]=0.0;
				}

	/*  Initialize the parameters  */

	sigma=1;  
	dev_sigma=5;   

	dev_x=0;  
	dev_y=0;  

	dev_theta=0;
	dev_sigma_theta=3;  

	/*       ML iteration            */     

	dev_sigma_change=1.0;
	Loop=1;
	while((Loop<Iteration && (dev_sigma_change>Terminate_ML || dev_sigma_theta_change>Terminate_ML )) || Loop<3)  
	{ 
		/*   Make rotated copies of the reference and normalize them by the initial reference */   

		pow_refer=0.0;
		for(k3=0;k3<nx;k3++)
			for(k4=0;k4<ny;k4++)
				pow_refer+=powf(refer[IDX(k3,k4,nx,ny)],2.0);

		for(k3=0;k3<nx;k3++)
			for(k4=0;k4<ny;k4++)
				RTrefer[IDX(k3,k4,nx,ny)]=refer[IDX(k3,k4,nx,ny)];

		for(k2=1;k2<Num_angles;k2++)
		{    

			rotate(nx,ny,angle[k2],refer,RTimage);

			pow_RT=0;
			for(i=0;i<nx;i++)
				for(j=0;j<ny;j++)
					pow_RT+=powf(RTimage[IDX(i,j,nx,ny)],2.0);

			for(k3=0;k3<nx;k3++)
				for(k4=0;k4<ny;k4++)
					RTrefer[IDX(k3,k4,nx,ny)+k2*nx*ny]=RTimage[IDX(k3,k4,nx,ny)]*sqrt(pow_refer/pow_RT);
		}

		/*    Calculate the statistical parameters of translation */

		for(k2=0;k2<Num_angles;k2++)
		{
			for(k3=0;k3<nx;k3++)
				for(k4=0;k4<ny;k4++)
					f[IDX(k3,k4,nx,ny)+k2*nx*ny]=0.0;

			for(k3=0;k3<nx;k3++)
				for(k4=0;k4<ny;k4++)
					if (powf((float)(indx[IDX(k3,k4,nx,ny)]-dev_x),Gau)+powf((float)(indy[IDX(k3,k4,nx,ny)]-dev_y),Gau)<20*20)
					{  st1=powf((float)(indx[IDX(k3,k4,nx,ny)]-dev_x),Gau)+powf((float)(indy[IDX(k3,k4,nx,ny)]-dev_y),Gau);
						st2=Gau*powf(dev_sigma,Gau);

						st3=powf((float)(angle[k2]-dev_theta),Gau);
						st4=Gau*powf(dev_sigma_theta,Gau);


						if(Num_angles>1)
						{
							st5=powf(dev_sigma,Gau)*dev_sigma_theta;
							st5=1.0/(powf(sqrtf(2*pi),3.0)*st5+1.0e-20);			  			 
							f[IDX(k3,k4,nx,ny)+k2*nx*ny]=st5*exp(-st1/(st2+1.0e-20)-st3/(st4+1.0e-20));
						}
						else     
						{  st5=powf(dev_sigma,Gau);
							st5=1.0/(powf(sqrtf(2*pi),2.0)*st5+1.0e-20);			  			 
							f[IDX(k3,k4,nx,ny)+k2*nx*ny]=st5*exp(-st1/(st2+1.0e-20));
						}

					}	
					else    f[IDX(k3,k4,nx,ny)+k2*nx*ny]=0;   	   
		}

		/*  Initialize the parameters that will be summation over all the particles */

		new_sigma=0;
		new_dev_sigma=0;

		new_dev_x=0;
		new_dev_y=0;

		new_dev_theta=0.0;
		new_sigma_theta=0.0;
		Num_images=0;

		for(k2=0;k2<Num_angles;k2++)	 
			for(k3=0;k3<nx;k3++)
				for(k4=0;k4<ny;k4++)
					new_refer[IDX(k3,k4,nx,ny)+k2*nx*ny]=0; 



		input[0]=fopen("stack_whit.binary","r");
		input[1]=fopen("stack_ctf.binary","r");
		
		size_t result;
		result = fread(temp_image1, sizeof(float)*nx*ny, 1, input[0]);
		if (result != 1) 
		{
			fputs ("Reading error",stderr);
			exit(3);
		}
		
		result = fread(temp_image2, sizeof(float)*nx*ny, 1, input[1]);
		if (result != 1) 
		{
			fputs ("Reading error",stderr);
			exit(3);
		}
		
		num_temp=Max_num;

		if(feof(input[0])!=0 && feof(input[1])!=0)
		{   printf("there are not particles extracted from the files \n");
			fflush(stdout);
			break;
		} 
		else while(num_temp==Max_num)
		{
			num_temp=0;
			while(feof(input[0])==0 && num_temp<Max_num)  
			{     
				for(i=0;i<nx;i++)
					for(j=0;j<ny;j++)
					{      Image[IDX(i,j,nx,ny)+num_temp*nx*ny]=temp_image1[IDX(i,j,nx,ny)];
						Image_CTF[IDX(i,j,nx,ny)+num_temp*nx*ny]=temp_image2[IDX(i,j,nx,ny)];
					}
				num_temp++;
				
				result = fread(temp_image1, sizeof(float)*nx*ny, 1, input[0]);
				if (result != 1) 
				{
					fputs ("Reading error",stderr);
					exit(3);
				}
				
				result = fread(temp_image2, sizeof(float)*nx*ny, 1, input[1]);
				if (result != 1) 
				{
					fputs ("Reading error",stderr);
					exit(3);
				}
			}                  

			Num_images+=num_temp;

			if(num_temp<Max_num)
			{   fclose(input[0]);
				fclose(input[1]);
			} 


			/*          Update  ML  Parameters          */	   

			for(k1=0;k1<num_temp;k1++)
			{         	   
				for(k3=0;k3<nx;k3++)
					for(k4=0;k4<ny;k4++)
						temp_image[IDX(k3,k4,nx,ny)]=Image[IDX(k3,k4,nx,ny)+k1*nx*ny];

				/*   Calculate cross correlation of the particles and the reference, using Fourier transform */
				cross_corr(nx,ny,Num_angles,temp_image,RTrefer,corr_image);

				max=-1.0e20;   
				for(k2=0;k2<Num_angles;k2++)
					for(i=0;i<nx;i++)
						for(j=0;j<ny;j++)
							if(corr_image[IDX(i,j,nx,ny)+k2*nx*ny]>=max) 
								max=corr_image[IDX(i,j,nx,ny)+k2*nx*ny];


				gama=0;		       
				for(k2=0;k2<Num_angles;k2++)
					for(k3=0;k3<nx;k3++)
						for(k4=0;k4<ny;k4++)
						{     weight[IDX(k3,k4,nx,ny)+k2*nx*ny]=exp((corr_image[IDX(k3,k4,nx,ny)+k2*nx*ny]-max)/(sigma*sigma))*f[IDX(k3,k4,nx,ny)+k2*nx*ny]; 

							gama+=weight[IDX(k3,k4,nx,ny)+k2*nx*ny];
						}

				likelihood+=log(gama)+max/(sigma*sigma+1.0e-20)-(pow_image+pow_refer)/(2*sigma*sigma+1.0e-20)-nx*ny*log(sigma);  	     

				for(k2=0;k2<Num_angles;k2++)
					for(k3=0;k3<nx;k3++)
						for(k4=0;k4<ny;k4++)
							weight[IDX(k3,k4,nx,ny)+k2*nx*ny]/=(gama+1.0e-20); 


				/*    Update the translation parameters   */
				corr=0;
				for(k2=0;k2<Num_angles;k2++)		 
					for(k3=0;k3<nx;k3++)    //  translation in x
						for(k4=0;k4<ny;k4++)  // translation in y
						{     			   			    
							new_dev_sigma+=(powf(indx[IDX(k3,k4,nx,ny)]-dev_x,Gau)*weight[IDX(k3,k4,nx,ny)+k2*nx*ny]+powf(indy[IDX(k3,k4,nx,ny)]-dev_y,Gau)*weight[IDX(k3,k4,nx,ny)+k2*nx*ny]);  
							new_dev_x+=(indx[IDX(k3,k4,nx,ny)]*weight[IDX(k3,k4,nx,ny)+k2*nx*ny]);
							new_dev_y+=(indy[IDX(k3,k4,nx,ny)]*weight[IDX(k3,k4,nx,ny)+k2*nx*ny]);

							new_dev_theta+=(angle[k2]*weight[IDX(k3,k4,nx,ny)+k2*nx*ny]);
							new_sigma_theta+=(powf(angle[k2]-dev_theta,Gau)*weight[IDX(k3,k4,nx,ny)+k2*nx*ny]); 

							corr+=(corr_image[IDX(k3,k4,nx,ny)+k2*nx*ny]*weight[IDX(k3,k4,nx,ny)+k2*nx*ny]);
						}

				/*         Update the noise std     */
				new_sigma+=(pow_image+pow_refer-2*corr);


				/*  Calculate the reference weighted by the probability, using Fourier transform */
				cross_corr(nx,ny,Num_angles,temp_image,weight,corr_gama);

				for(k2=0;k2<Num_angles; k2++) 
					for(i=0;i<nx;i++)
						for(j=0;j<ny;j++)
							new_refer[IDX(i,j,nx,ny)+k2*nx*ny]+=corr_gama[IDX(i,j,nx,ny)+k2*nx*ny];


				/*  If there are CTF corrected images, bring back the corresponding  references  */

				if(CTF==1)
				{    
					for(i=0;i<nx;i++)
						for(j=0;j<ny;j++)
							temp_image[IDX(i,j,nx,ny)]=Image_CTF[IDX(i,j,nx,ny)+k1*nx*ny];

					cross_corr(nx,ny,Num_angles,temp_image,weight,corr_gama);

					for(k2=0;k2<Num_angles; k2++) 
						for(i=0;i<nx;i++)
							for(j=0;j<ny;j++)
							{      final_refer[IDX(i,j,nx,ny)+k2*nx*ny+num_refer*nx*ny*Num_angles]+=corr_gama[IDX(i,j,nx,ny)+k2*nx*ny];
								if(fmod(k1*1.0,2.0)>=0.5)
									final_refer1[IDX(i,j,nx,ny)+k2*nx*ny+num_refer*nx*ny*Num_angles]+=corr_gama[IDX(i,j,nx,ny)+k2*nx*ny];
								else
									final_refer2[IDX(i,j,nx,ny)+k2*nx*ny+num_refer*nx*ny*Num_angles]+=corr_gama[IDX(i,j,nx,ny)+k2*nx*ny];
							}
				}			       

			}   
		}        // end of iterations over images    


		/*   Rotate the reference back   */  


		for(k2=0;k2<Num_angles;k2++)
			for(i=0;i<nx;i++)
				for(j=0;j<ny;j++)
					new_refer[IDX(i,j,nx,ny)+k2*nx*ny]/=Num_images;	

		rot_refer(Num_angles,angle,nx,ny,new_refer,refer);

		/*   Apply symmetry if applicable   */

		if(Symmetry>1)
			Symmetrize(nx,ny,refer);

		/*   Low pass filtering the reference to reduce the noise */


		pow_refer=0;
		for(i=0;i<nx;i++)
			for(j=0;j<ny;j++)
				pow_refer+=powf(refer[IDX(i,j,nx,ny)],2.0);

		if(lp_radius>0.0 && lp_method>0)
			low_pass(nx,ny,refer,lp_radius,lp_method);  		    

		pow_RT=0;   
		for(i=0;i<nx;i++)
			for(j=0;j<ny;j++)
				pow_RT+=powf(refer[IDX(i,j,nx,ny)],2.0); 

		for(k3=0;k3<nx;k3++)
			for(k4=0;k4<ny;k4++)
				refer[IDX(k3,k4,nx,ny)]=refer[IDX(k3,k4,nx,ny)]*sqrt(pow_refer/pow_RT);

		max=-1.0e20;
		min=-max;
		for(i=0;i<nx;i++)
			for(j=0;j<ny;j++)
			{ if(refer[IDX(i,j,nx,ny)]>=max) max=refer[IDX(i,j,nx,ny)];
				else if(refer[IDX(i,j,nx,ny)]<=min) min=refer[IDX(i,j,nx,ny)];
			}

		for(i=0;i<nx;i++)
			for(j=0;j<ny;j++)
				temp_image[IDX(i,j,nx,ny)]=contrast*(refer[IDX(i,j,nx,ny)]-min)*256/(max-min)-(contrast-1.0)*128.0;  

		// printf("Loop=%d max=%f min=%f  refer=%f \n", Loop, max,min,refer[100]);
		//  fflush(stdout);


		mrcImage::mrcHeader *header2 = mrcImage::headerFromData(nx,ny,2,(char*)temp_image);
		char outputfile [50];
		sprintf(outputfile,"./ML/ML_reference_%03d.mrc",Loop);
		mrcImage(header2,(char*)temp_image,outputfile);
		printf("File %s written.\n",outputfile);
		fprintf(results,"# IMAGE: %s <Reference Map %03d>\n",outputfile,Loop);

		mask(mask_radius,mask_radius,nx,ny,refer); 

		dev_sigma_change=fabs(dev_sigma-powf(new_dev_sigma/(Gau*Num_images), 1.0/Gau));
		dev_sigma_theta_change=fabs(dev_sigma_theta-powf(new_sigma_theta/(Num_images), 1.0/Gau));

		sigma=sqrt(new_sigma/(Num_images*nx*ny));	 
		dev_sigma=powf(new_dev_sigma/(Gau*Num_images), 1.0/Gau);		 
		dev_x=new_dev_x/Num_images;
		dev_y=new_dev_y/Num_images;	

		dev_theta=new_dev_theta/Num_images;
		dev_sigma_theta=powf(new_sigma_theta/(Num_images), 1.0/Gau);

		for(i=0;i<nx;i++)
			for(j=0;j<ny;j++)
				Image_refer[IDX(i,j,nx,ny)+Loop*nx*ny]=refer[IDX(i,j,nx,ny)]; 

		printf("::%5d %10.6f %10.6f %10.6f %12.6f ",Loop,sigma,dev_x,dev_y,dev_sigma);
		printf("%12.6f %12.6f\n",dev_theta,dev_sigma_theta);

		// printf("<<@evaluate>>\n");
		fflush(results);
		fflush(stdout);


		Loop++;
		num_refer++;


	}	//  end of iteration of Loops	 

	if(Loop>=Iteration)
		printf(":Iteration maximum of %d reached.\n",Iteration);
	if(dev_sigma_change<=Terminate_ML && dev_sigma_theta_change<=Terminate_ML )
		printf(":ML termination due to convergence (dev-change below threshold of %f).\n",Terminate_ML);

	printf(":: \n");


	/*    bring back the CTF corrected reference */




	if(CTF==1)  
		for(m=1;m<num_refer;m++)
		{   /*   Rotate the reference back    */  


			for(k2=0;k2<Num_angles;k2++)
				for(i=0;i<nx;i++)
					for(j=0;j<ny;j++)
						new_refer[IDX(i,j,nx,ny)+k2*nx*ny]=final_refer[IDX(i,j,nx,ny)+k2*nx*ny+m*nx*ny*Num_angles]/Num_images;			

			rot_refer(Num_angles, angle,nx,ny,new_refer,refer);


			for(k2=0;k2<Num_angles;k2++)
				for(i=0;i<nx;i++)
					for(j=0;j<ny;j++)
						new_refer[IDX(i,j,nx,ny)+k2*nx*ny]=final_refer1[IDX(i,j,nx,ny)+k2*nx*ny+m*nx*ny*Num_angles]/Num_images;			

			rot_refer(Num_angles, angle,nx,ny,new_refer,refer1); 


			for(k2=0;k2<Num_angles;k2++)
				for(i=0;i<nx;i++)
					for(j=0;j<ny;j++)
						new_refer[IDX(i,j,nx,ny)+k2*nx*ny]=final_refer2[IDX(i,j,nx,ny)+k2*nx*ny+m*nx*ny*Num_angles]/Num_images;			

			rot_refer(Num_angles, angle,nx,ny,new_refer,refer2);

			/*   Apply symmetry if applicable    */   

			if(Symmetry>1)
			{  Symmetrize(nx,ny,refer);
				Symmetrize(nx,ny,refer1);
				Symmetrize(nx,ny,refer2);
			}


			for(i=0;i<nx;i++)
				for(j=0;j<ny;j++)
				{  temp_refer[IDX(i,j,nx,ny)]=refer[IDX(i,j,nx,ny)];  
					temp_refer1[IDX(i,j,nx,ny)]=refer1[IDX(i,j,nx,ny)];  
					temp_refer2[IDX(i,j,nx,ny)]=refer2[IDX(i,j,nx,ny)];
				}

			max=-1.0e20;
			min=-max;
			for(i=0;i<nx;i++)
				for(j=0;j<ny;j++)
				{   if(refer[IDX(i,j,nx,ny)]>=max) max=refer[IDX(i,j,nx,ny)];
					else if(refer[IDX(i,j,nx,ny)]<=min) min=refer[IDX(i,j,nx,ny)];
				}


			for(i=0;i<nx;i++)
				for(j=0;j<ny;j++)
					temp_image[IDX(i,j,nx,ny)]=contrast*(refer[IDX(i,j,nx,ny)]-min)*256/(max-min)-(contrast-1.0)*128.0;  

			mrcImage::mrcHeader *header3 = mrcImage::headerFromData(nx,ny,2,(char*)temp_image);
			char outputfile [50];
			sprintf(outputfile,"./ML/ML_reference_CTF_%03d.mrc",m);
			mrcImage(header3,(char*)temp_image,outputfile);
			printf("File %s written.\n",outputfile);
			fprintf(results,"# IMAGE: %s <CTF-corr. Reference Map %03d>\n",outputfile,m);
			fflush(results);
			// printf("<<@evaluate>>\n");
			fflush(stdout);



			mask(mask_radius,mask_radius,nx,ny,refer); 

			for(i=0;i<nx;i++)
				for(j=0;j<ny;j++)
					Image_refer_CTF[IDX(i,j,nx,ny)+m*nx*ny]=refer[IDX(i,j,nx,ny)];	

		}		  


	/*          Write the references into a file  "output1.pgm " */         


	for(k=0;k<Iteration;k++)
	{  max=-1e20;min=1e20; 
		for(i=0;i<nx;i++)
			for(j=0;j<ny;j++)
				if(Image_refer[IDX(i,j,nx,ny)+k*nx*ny]>=max) max=Image_refer[IDX(i,j,nx,ny)+k*nx*ny];
				else if(Image_refer[IDX(i,j,nx,ny)+k*nx*ny]<=min) min=Image_refer[IDX(i,j,nx,ny)+k*nx*ny];

		for(i=0;i<nx;i++)
			for(j=0;j<ny;j++)
				Image_refer[IDX(i,j,nx,ny)+k*nx*ny]=(Image_refer[IDX(i,j,nx,ny)+k*nx*ny]-min)*256/(max-min);  
	}     


	for(m=0;m<Iteration/5;m++)
	{  for(i=0;i<nx;i++)
		{  for(k=0;k<5;k++)
			{   for(j=0;j<ny;j++)
				fprintf(output[0],"%d ",(int)(Image_refer[IDX(i,j,nx,ny)+(k+m*5)*nx*ny]));   
				fprintf(output[0],"%d %d  ", 256, 256);
			}
			fprintf(output[0],"\n");
		}


		for(j=0;j<5*(ny+2);j++)
			fprintf(output[0],"%d ",256);

		fprintf(output[0],"\n");

		for(j=0;j<5*(ny+2);j++)
			fprintf(output[0],"%d ",256);

		fprintf(output[0],"\n");

	}  

	fclose(output[0]);     

	/*  write the corrected references into a file "output2.pgm" */

	if(CTF==1)
	{
		for(k=0;k<Iteration;k++)
		{  
			max=-1e20;min=1e20; 
			for(i=0;i<nx;i++)
				for(j=0;j<ny;j++)
					if(Image_refer_CTF[IDX(i,j,nx,ny)+k*nx*ny]>=max) max=Image_refer_CTF[IDX(i,j,nx,ny)+k*nx*ny];
					else if(Image_refer_CTF[IDX(i,j,nx,ny)+k*nx*ny]<=min) min=Image_refer_CTF[IDX(i,j,nx,ny)+k*nx*ny];

			for(i=0;i<nx;i++)
				for(j=0;j<ny;j++)
					Image_refer_CTF[IDX(i,j,nx,ny)+k*nx*ny]=(Image_refer_CTF[IDX(i,j,nx,ny)+k*nx*ny]-min)*256/(max-min);  

		}     


		for(m=0;m<Iteration/5;m++)
		{   
			for(i=0;i<nx;i++)
			{  for(k=0;k<5;k++)
				{    for(j=0;j<ny;j++)
					fprintf(output[1],"%d ",(int)(Image_refer_CTF[IDX(i,j,nx,ny)+(k+m*5)*nx*ny]));   
					fprintf(output[1],"%d %d  ", 256, 256);
				}
				fprintf(output[1],"\n");
			}

			for(j=0;j<5*(ny+2);j++)
				fprintf(output[1],"%d ",256);

			fprintf(output[1],"\n");

			for(j=0;j<5*(ny+2);j++)
				fprintf(output[1],"%d ",256);

			fprintf(output[1],"\n");
		}  
	}


	fclose(output[1]);

	/*    Return the references and aligned particles  */


	for(i=0;i<nx;i++)
		for(j=0;j<ny;j++)
			refer[IDX(i,j,nx,ny)]=temp_refer[IDX(i,j,nx,ny)];  

	for(i=0;i<nx;i++)
		for(j=0;j<ny;j++)
			refer1[IDX(i,j,nx,ny)]=temp_refer1[IDX(i,j,nx,ny)];  

	for(i=0;i<nx;i++)
		for(j=0;j<ny;j++)
			refer2[IDX(i,j,nx,ny)]=temp_refer2[IDX(i,j,nx,ny)];  	 			  




	/*    Bring back the particles and references at the peaks   */

	/*  num_refer=0;
			for(i=0;i<nx;i++)
			for(j=0;j<ny;j++)
			{  refer[IDX(i,j,nx,ny)]=0.0;
			refer1[IDX(i,j,nx,ny)]=0.0;
			refer2[IDX(i,j,nx,ny)]=0.0;
			}  
	 */

	/*
		 float *SFimage,devi;
		 SFimage=(float *)malloc(sizeof(float)*nx*ny);

		 if(CTF==1)   
		 for(k1=0;k1<Num_images;k1++)
		 {   

		 for(i=0;i<nx;i++)
		 for(j=0;j<ny;j++)
		 temp_image[IDX(i,j,nx,ny)]=Image_CTF[IDX(i,j,nx,ny)+k1*nx*ny];

		 translate(nx,ny, -sfx[k1],-sfy[k1],temp_image,SFimage);
		 rotate(nx,ny,-rota[k1],SFimage,RTimage);


		 mean=0.0;
		 for(i=0;i<nx;i++)
		 for(j=0;j<ny;j++)
		 mean+=RTimage[IDX(i,j,nx,ny)];
		 mean/=(nx*ny);

		 devi=0.0;
		 for(i=0;i<nx;i++)
		 for(j=0;j<ny;j++)
		 devi+=powf(RTimage[IDX(i,j,nx,ny)],2.0);
		 devi=sqrt(devi/(nx*ny));

		 for(i=0;i<nx;i++)
		 for(j=0;j<ny;j++)
		 RTimage[IDX(i,j,nx,ny)]/=devi;

		 for(i=0;i<nx;i++)
		 for(j=0;j<ny;j++)
		 {   Image_particle[IDX(i,j,nx,ny)+k1*nx*ny]=RTimage[IDX(i,j,nx,ny)]; 
		 refer[IDX(i,j,nx,ny)]+=RTimage[IDX(i,j,nx,ny)];
		 if(fmod(k1*1.0,2.0)>=0.5)
		 refer1[IDX(i,j,nx,ny)]+=RTimage[IDX(i,j,nx,ny)];
		 else
		 refer2[IDX(i,j,nx,ny)]+=RTimage[IDX(i,j,nx,ny)];
		 }  

		 }


	 */



	free(indx); free(indy); 
	free(final_refer); 
	free(final_refer1);  
	free(final_refer2);
	free(temp_image); 
	free(temp_refer); 
	free(temp_refer1);  
	free(temp_refer2);
	free(RTimage);  
	free(RTrefer);  
	free(weight);
	free(new_refer);
	free(Image_refer);
	free(Image_refer_CTF);
	free(corr_image);
	free(corr_gama);
	free(angle);
	free(f);
	free(Image);
	free(Image_CTF);
	free(temp_image1);
	free(temp_image2);




}			     

