/*
 *
 *  Created by Xiangyan Zeng on 1/1/2007
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */

#include <common.h>

#ifndef pi
#define pi  3.141592654
#endif


void envelop(int sx,int sy, float *refer, float A, float B, float *FSC)
{    
	int i,j,k,m;

	float WL,STEPR,THETATR,RAD,env;
	int   ISIZE=sx;



	fftwf_complex *in, *out;
	fftwf_plan p1,p2;




	in=(fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*sx*sy);
	out=( fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*sx*sy);


	CS=CS*(10000000);
	KV=KV*1000;
	WL=12.3/sqrt(KV+KV*KV/(1000000.0));

	STEPR=DSTEP*(10000)/XMAG;
	THETATR=WL/(STEPR*ISIZE); 



	/*   FFT transform */      
	for(i=0;i<sx;i++)
		for(j=0;j<sy;j++)
		{    
			in[IDX(i,j,sx,sy)][0]=refer[IDX(i,j,sx,sy)]*powf(-1,(i+j)*1.0); 
			in[IDX(i,j,sx,sy)][1]=0.0;
		} 


	p1=fftwf_plan_dft_2d(sx,sy,in,out,FFTW_FORWARD,FFTW_ESTIMATE);
	p2=fftwf_plan_dft_2d(sx,sy,out,in,FFTW_BACKWARD,FFTW_ESTIMATE);



	fftwf_execute(p1);
	fftwf_destroy_plan(p1);


	/*    Apply envelope   */

	float rmax=sx*DSTEP*10000/(XMAG*RESMAX);
	float rmin=sx*DSTEP*10000/(XMAG*RESMIN);

	printf("rmax=%f rmin=%f \n", rmax,rmin);


	for(i=0;i<sx;i++) 
		for(j=0;j<sy;j++)
		{  

			RAD = sqrtf((i-sx/2)*(i-sx/2)*1.0+(j-sy/2)*(j-sy/2)*1.0)+1;
			m=(int)RAD;


			if(m<sx/2) // rmax  && m>rmin)
			{

				//    RAD=RAD*THETATR;	      
				//    env=B*RAD*RAD;

				//    out[j+i*sy][0]=out[j+i*sy][0]*(A+(1-A)*sqrt(2*fabs(FSC[m])/(1+fabs(FSC[m]))))*exp(-env);
				//    out[j+i*sy][1]=out[j+i*sy][1]*(A+(1-A)*sqrt(2*fabs(FSC[m])/(1+fabs(FSC[m]))))*exp(-env);

				env=B/(4*RAD*RAD); 
				out[IDX(i,j,sx,sy)][0]=out[IDX(i,j,sx,sy)][0]*exp(env);
				out[IDX(i,j,sx,sy)][1]=out[IDX(i,j,sx,sy)][1]*exp(env);

			}
			else
			{
				out[IDX(i,j,sx,sy)][0]=0;
				out[IDX(i,j,sx,sy)][1]=0;
			}

		}  


	/*  IFFT transform  */


	fftwf_execute(p2);
	fftwf_destroy_plan(p2);




	for(i=0;i<sx;i++)
		for(j=0;j<sy;j++)
			refer[IDX(i,j,sx,sy)]=in[IDX(i,j,sx,sy)][0]*powf(-1,(i+j)*1.0);  



	fftwf_free(in); fftwf_free(out);



}
