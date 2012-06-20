/*
 *
 *  Created by Xiangyan Zeng on 1/1/2007
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */
#ifndef pi
#define pi 3.141592654
#endif

#include <common.h>

void resolution(int sx1,int sy1,float *image1,float *image2,float *FRC)
{     
	FILE *output[2];    
	int  sx2=sx1, sy2=sy1, sx=sx1, sy=sy1;
	float temp, *r, r_step, *ref1, *ref2, max;

	float   mean;

	int   ISIZE=sx; 

	fftwf_complex *in1, *out1, *in2, *out2;
	fftwf_plan p1,p2;
	int k2,k3,k4,i,j,k,m,im,jm,km,*num;
	float  *amp1, *amp2, *corr,  *F1, *F2, *F12,  max12,min, max1, max2,max3;  

	float Step_angle=1;
	int Num_angles=30;
	float *RTimage,*SFimage, *RTrefer,*corr_big, pow_RT,angle;
	RTimage=(float *)calloc(sx2*sy2,sizeof(float));
	RTrefer=(float *)calloc(sx2*sy2*Num_angles,sizeof(float));
	SFimage=(float *)calloc(sx2*sy2*Num_angles,sizeof(float));
	corr_big=(float *)calloc(sx2*sy2*Num_angles,sizeof(float));


	in1=(fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*sx*sy);
	out1=( fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*sx*sy);
	in2=(fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*sx*sy);
	out2=( fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*sx*sy);
	amp1=(float *)calloc(sx*sy,sizeof(float));  
	amp2=(float *)calloc(sx*sy,sizeof(float));


	ref1=(float *)calloc(sx2*sy2,sizeof(float));
	ref2=(float *)calloc(sx2*sy2,sizeof(float));

	corr=(float *)calloc(sx*sy,sizeof(float));

	F1=(float *)calloc(sx,sizeof(float));
	F2=(float *)calloc(sx,sizeof(float));
	F12=(float *)calloc(sx,sizeof(float)); 

	num=(int *)calloc(sx,sizeof(int));



	output[0]=fopen("./SCRATCH/FSC.dat","w");
	output[1]=fopen("./SCRATCH/AMP.dat","w");


	/*   Cut out the center area of the masked references */

	for(i=0;i<sx2;i++)
		for(j=0;j<sy2;j++)
		{	  

			ref1[IDX(i,j,sx2,sy2)]=image1[IDX(i+(sx1-sx2)/2,j+(sy1-sy2)/2,sx1,sy1)];  
			ref2[IDX(i,j,sx2,sy2)]=image2[IDX(i+(sx1-sx2)/2,j+(sy1-sy2)/2,sx1,sy1)];  
		}



	/*  Align the two references */

	for(k2=0;k2<Num_angles;k2++)
	{    
		angle=(float)(k2)*Step_angle-Num_angles*Step_angle/2.0;

		rotate(sx2,sy2,angle,ref1,RTimage);

		mean=0;	 
		for(i=0;i<sx2;i++)
			for(j=0;j<sy2;j++)
				mean+=RTimage[IDX(i,j,sx2,sy2)];

		mean/=(sx2*sy2);	 


		pow_RT=0;
		for(i=0;i<sx2;i++)
			for(j=0;j<sy2;j++)
			{   RTimage[IDX(i,j,sx2,sy2)]=RTimage[IDX(i,j,sx2,sy2)]-mean;
				pow_RT+=powf(RTimage[IDX(i,j,sx2,sy2)],2.0);
			}


		for(k3=0;k3<sx2;k3++)
			for(k4=0;k4<sy2;k4++)
				RTrefer[IDX(k3,k4,sx2,sy2)+k2*sx2*sy2]=RTimage[IDX(k3,k4,sx2,sy2)]/sqrt(pow_RT);

	}


	cross_corr(sx2,sy2,Num_angles,ref2,RTrefer,corr_big);


	max=-1.0e20;
	for(k2=0;k2<Num_angles;k2++)
		for(k3=0;k3<sx2;k3++)
			for(k4=0;k4<sy2;k4++)
				if(corr_big[IDX(k3,k4,sx2,sy2)+k2*sx2*sy2]>max) 
				{   im=k3;  
					jm=k4; 
					km=k2;  
					max=corr_big[IDX(k3,k4,sx2,sy2)+k2*sx2*sy2]; 
				}


	if(im>sx2/2)  im=im-sx2;
	if(jm>sy2/2)  jm=jm-sy2;	

	angle=(float)(km)*Step_angle-Num_angles*Step_angle/2;
	printf("sx2=%d sy2=%d trans x=%d y=%d  k=%d angle=%f \n",sx2,sy2,im,jm,km,angle);

	for(k3=0;k3<sx2;k3++)
		for(k4=0;k4<sy2;k4++)
			ref1[IDX(k3,k4,sx2,sy2)]=RTrefer[IDX(k3,k4,sx2,sy2)+km*sx2*sy2];  


	if(fabs(im*1.0)>0 || fabs(jm*1.0)>0)		
		translate(sx2,sy2, im,jm,ref1,SFimage);
	else
		for(k3=0;k3<sx2;k3++)
			for(k4=0;k4<sy2;k4++)
				SFimage[IDX(k3,k4,sx2,sy2)]=ref1[IDX(k3,k4,sx2,sy2)]; 


	if(fabs(angle)>0.1)
		rotate(sx2,sy2,angle,SFimage,ref1);  
	else 
		for(k3=0;k3<sx2;k3++)
			for(k4=0;k4<sy2;k4++)
				ref1[IDX(k3,k4,sx2,sy2)]=SFimage[IDX(k3,k4,sx2,sy2)];  




	/*  Cut out the center area of the aligned references */

	for(i=0;i<sx;i++)
		for(j=0;j<sy;j++)
		{	  
			in1[IDX(i,j,sx,sy)][0]=ref1[IDX(i+(sx2-sx)/2,j+(sy2-sy)/2,sx2,sy2)]; //  
			in1[IDX(i,j,sx,sy)][1]=0;

			in2[IDX(i,j,sx,sy)][0]=ref2[IDX(i+(sx2-sx)/2,j+(sy2-sy)/2,sx2,sy2)]; //   
			in2[IDX(i,j,sx,sy)][1]=0; 
		}  



	/*  Normalize the references */      


	mean=0;
	for(i=0;i<sx;i++)
		for(j=0;j<sy;j++)
			mean+=in1[IDX(i,j,sx,sy)][0];
	mean/=(sx*sy);
	for(i=0;i<sx;i++)
		for(j=0;j<sy;j++)
			in1[IDX(i,j,sx,sy)][0]=(in1[IDX(i,j,sx,sy)][0]-mean)*powf(-1,(i+j)*1.0);

	mean=0;
	for(i=0;i<sx;i++)
		for(j=0;j<sy;j++)
			mean+=in2[IDX(i,j,sx,sy)][0];
	mean/=(sx*sy);
	for(i=0;i<sx;i++)
		for(j=0;j<sy;j++)
			in2[IDX(i,j,sx,sy)][0]=(in2[IDX(i,j,sx,sy)][0]-mean)*powf(-1,(i+j)*1.0);



	p1=fftwf_plan_dft_2d(sx,sy,in1,out1,FFTW_FORWARD,FFTW_ESTIMATE);
	p2=fftwf_plan_dft_2d(sx,sy,in2,out2,FFTW_FORWARD,FFTW_ESTIMATE);

	fftwf_execute(p1);
	fftwf_destroy_plan(p1);
	fftwf_execute(p2);
	fftwf_destroy_plan(p2);



	/*  Get amplitude and phase of FFT image   */
	for(i=0;i<sx;i++)
		for(j=0;j<sy;j++)
		{
			amp1[IDX(i,j,sx,sy)]=sqrtf(pow(out1[IDX(i,j,sx,sy)][0],2)+pow(out1[IDX(i,j,sx,sy)][1],2));

			amp2[IDX(i,j,sx,sy)]=sqrtf(pow(out2[IDX(i,j,sx,sy)][0],2)+pow(out2[IDX(i,j,sx,sy)][1],2));

			corr[IDX(i,j,sx,sy)]=(out1[IDX(i,j,sx,sy)][0]*out2[IDX(i,j,sx,sy)][0]+out1[IDX(i,j,sx,sy)][1]*out2[IDX(i,j,sx,sy)][1]);

		} 


	/*   Get  Fourier Ring Correlation  */

	for(i=0;i<sx; i++)
	{  F1[i]=0;
		F2[i]=0;
		F12[i]=0;
		num[i]=0;
	}


	/* 
		 float R; 
		 int ii,jj;
		 for(i=0;i<sx;i++)
		 for(j=0;j<sy;j++) 
		 {   ii=i-sx/2;
		 jj=j-sy/2;

		 m=(int)(sqrtf((i-sx/2)*(i-sx/2)*1.0+(j-sy/2)*(j-sy/2)*1.0)+0.5);

		 F1[m]+=powf(amp1[IDX(i,j,sx,sy)],2.0);
		 F2[m]+=powf(amp2[IDX(i,j,sx,sy)],2.0);
		 F12[m]+=corr[IDX(i,j,sx,sy)];
		 num[m]++;

		 }

	 */ 



	int rad;
	int ix,iy;

	for(rad=1;rad<sx/2;rad++)
	{   for(i=0;i<(int)(2*pi*rad);i++)
		{   ix=(int)(rad*cos((float)i/(float)rad)+sx/2);  
			iy=(int)(rad*sin((float)i/(float)rad)+sy/2); 

			F1[rad]+=powf(amp1[IDX(ix,iy,sx,sy)],2.0);
			F2[rad]+=powf(amp2[IDX(ix,iy,sx,sy)],2.0);
			F12[rad]+=corr[IDX(ix,iy,sx,sy)];
			num[rad]++;

		}
	}





	for(i=0;i<sx/2;i++)   
		if(F1[i]>0 && F2[i]>0)
			FRC[i]=F12[i]/(sqrtf(F1[i])*sqrtf(F2[i]));
		else
			FRC[i]=1;  



	/*   output  the result  */

	m=sx/2;
	max12=-1.0e20;   
	for(i=0;i<sx/2;i++)
		if(FRC[i]>max12) max12=FRC[i];


	max1=-1.0e20;   
	for(i=0;i<sx/2;i++)
		if(F1[i]>max1) max1=F1[i];


	for(i=0;i<sx/2;i++)
	{    
		fprintf(output[0],"   %f     %f \n",   1.0/((float)(ISIZE*DSTEP*10000)/(float)(XMAG*(i+1)*1)), FRC[i]);
		fprintf(output[1]," %f     %f \n", 1.0/((float)(ISIZE*DSTEP*10000)/(float)(XMAG*(i+1)*1)), sqrt(F1[i]/max1));

	}



	fclose(output[0]); 
	fclose(output[1]); 

	fftwf_free(in1); fftwf_free(out1);
	fftwf_free(in2); fftwf_free(out2);  

	free(RTimage);
	free(RTrefer);
	free(SFimage);
	free(corr_big);  
	free(amp1);
	free(amp2);
	free(ref1);
	free(ref2);
	free(corr);
	free(F1);
	free(F2);
	free(num);  


}
