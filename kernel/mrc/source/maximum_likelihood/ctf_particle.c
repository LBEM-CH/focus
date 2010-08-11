/*
 *
 *  Created by Xiangyan Zeng on 1/1/2007
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */



#define  pi      3.1415926

#include <common.h>


void ctfapply(int num_images, int sx,int sy, float *Image1)
{    
	int i,j,k,m,it;

	float WL,STEPR,THETATR,RAD,ANGLE,C1,C2,ANGDIF,CNTRST,CHI,DF,PHACON,ANGSPT,CCOS, AMPCON,CS1,KV1;
	int   ISIZE=sx;
	float *ctf, *phase,SNR, max1,min1,max2,min2;
	float *amp;
	float power_out1,power_out2,mean;



	fftwf_complex *in, *out;
	fftwf_plan p1,p2;


	amp=(float *)malloc(sizeof(float)*sx*sy);
	phase=(float *)malloc(sizeof(float)*sx*sy);
	ctf=(float *)malloc(sizeof(float)*sx*sy);


	in=(fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*sx*sy);
	out=( fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*sx*sy);


	CS1=CS*(10000000);
	KV1=KV*1000;
	WL=12.3/sqrt(KV1+KV1*KV1/(1000000.0));

	STEPR=DSTEP*(10000)/XMAG;
	THETATR=WL/(STEPR*ISIZE); 


	//   PHACON=0.9975;
	//   AMPCON=sqrt(1-PHACON*PHACON);



	AMPCON=0.07;
	PHACON=sqrt(1-AMPCON*AMPCON);

	/*    Get ctf   */

	SNR=0.01;
	for(i=0;i<sx;i++)
	{  
		for(j=0;j<sy;j++)
		{  
			RAD = sqrtf((i-sx/2)*(i-sx/2)*1.0+(j-sy/2)*(j-sy/2)*1.0);
			ANGLE=RAD*THETATR;
			ANGSPT=atan2(((j-sy/2)*1.0),((i-sx/2)*1.0));
			C1=2*pi*ANGLE*ANGLE/(2.0*WL);
			C2=-C1*CS1*ANGLE*ANGLE/2.0;
			ANGDIF=ANGSPT-ANGAST*pi/180;
			CCOS=cos(2*ANGDIF);
			if(i==sx/2 && j==sy/2) ctf[IDX(i,j,sx,sy)]=1.0;
			else if(DIFMID1==0.0 || DIFMID2==0.0 )
				ctf[IDX(i,j,sx,sy)]=1.0;
			else
			{
				DF=0.5*(DIFMID1+DIFMID2+CCOS*(DIFMID1-DIFMID2));
				CHI=C1*DF+C2;
				ctf[IDX(i,j,sx,sy)]=-sin(CHI)*PHACON-cos(CHI)*AMPCON;

			}	 
		}  
	}



	/* Iteratively Correct the CTF of each particle */   

	for(it=0;it<num_images;it++)
	{     
		mean=0.0;
		for(i=0;i<sx;i++)
			for(j=0;j<sy;j++)
				mean+=Image1[IDX(i,j,sx,sy)+it*sx*sy];

		mean/=(sx*sy);
		for(i=0;i<sx;i++)
			for(j=0;j<sy;j++)      
			{   
				in[IDX(i,j,sx,sy)][0]=(Image1[IDX(i,j,sx,sy)+it*sx*sy]-mean)*pow(-1,(i+j)*1.0); 
				in[IDX(i,j,sx,sy)][1]=0;
			} 


		p1=fftwf_plan_dft_2d(sx,sy,in,out,FFTW_FORWARD,FFTW_ESTIMATE);
		p2=fftwf_plan_dft_2d(sx,sy,out,in,FFTW_BACKWARD,FFTW_ESTIMATE);



		fftwf_execute(p1);
		fftwf_destroy_plan(p1);



		/*  CTF correction of FFT image   */

		for(i=0;i<sx;i++)
			for(j=0;j<sy;j++)
			{   out[IDX(i,j,sx,sy)][0]=out[IDX(i,j,sx,sy)][0]*ctf[IDX(i,j,sx,sy)]/(powf(ctf[IDX(i,j,sx,sy)],2.0)+0.04);
				out[IDX(i,j,sx,sy)][1]=out[IDX(i,j,sx,sy)][1]*ctf[IDX(i,j,sx,sy)]/(powf(ctf[IDX(i,j,sx,sy)],2.0)+0.04); 
			}	       


		/*  IFFT transform  */

		fftwf_execute(p2);
		fftwf_destroy_plan(p2);


		max2=-1.0e20;  min2=-max2;
		for(i=0;i<sx;i++)
			for(j=0;j<sy;j++)
			{
				amp[IDX(i,j,sx,sy)]=in[IDX(i,j,sx,sy)][0]*pow(-1,(i+j)*1.0); 
				if(max2<amp[IDX(i,j,sx,sy)]) max2=amp[IDX(i,j,sx,sy)];
				if(min2>amp[IDX(i,j,sx,sy)]) min2=amp[IDX(i,j,sx,sy)];
			}


		for(i=0;i<sx;i++)
			for(j=0;j<sy;j++)
				//   Image1[IDX(i,j,sx,sy)]=amp[IDX(i,j,sx,sy)]; 
				Image1[IDX(i,j,sx,sy)+it*sx*sy]=(amp[IDX(i,j,sx,sy)]-min2)/(max2-min2)*255+1;  

	}

	fftwf_free(in); fftwf_free(out);
	free(phase); free(ctf);


}
