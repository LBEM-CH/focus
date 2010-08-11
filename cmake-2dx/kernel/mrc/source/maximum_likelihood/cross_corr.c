/*
 *
 *  Created by Xiangyan Zeng on 1/1/2007
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */


/*  Using cross-correlation based on FFT  to search for translation parameters shift_x and shift_y   
		nx, ny  :   dimension of Images
Image1  :   reference image 
Image2  :   target image
 */ 

#include <common.h>

void cross_corr(int nx,int ny,int Num_angles,  float *Image1, float *Image2, float *corr)
{   


	fftwf_complex *in,*out,*in1,*out1,*in2,*out2;
	fftwf_plan p1,p2,p3;
	int i,j,k,k3,m;



	in=(fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*nx*ny);
	out=(fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*nx*ny);
	in1=(fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*nx*ny);
	out1=(fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*nx*ny);
	in2=(fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*nx*ny);
	out2=(fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*nx*ny);





	for(k3=0;k3<Num_angles;k3++)
	{
		for(i=0;i<nx;i++)
			for(j=0;j<ny;j++)
			{   in1[IDX(i,j,nx,ny)][0]=Image1[IDX(i,j,nx,ny)]; // *pow(-1,(i+j)*1.0); 
				in1[IDX(i,j,nx,ny)][1]=0.0;
				in2[IDX(i,j,nx,ny)][0]=Image2[IDX(i,j,nx,ny)+k3*nx*ny]; // *pow(-1,(i+j)*1.0); 
				in2[IDX(i,j,nx,ny)][1]=0.0; 
			}



		p1=fftwf_plan_dft_2d(nx,ny,in1,out1,FFTW_FORWARD,FFTW_ESTIMATE);
		p2=fftwf_plan_dft_2d(nx,ny,in2,out2,FFTW_FORWARD,FFTW_ESTIMATE);
		p3=fftwf_plan_dft_2d(nx,ny,out,in,FFTW_BACKWARD,FFTW_ESTIMATE);



		fftwf_execute(p1);
		fftwf_destroy_plan(p1);
		fftwf_execute(p2);
		fftwf_destroy_plan(p2); 





		/*  Get mutiplication of two FFT images*/
		for(i=0;i<nx;i++)
			for(j=0;j<ny;j++)
			{  out[IDX(i,j,nx,ny)][0]=out1[IDX(i,j,nx,ny)][0]*out2[IDX(i,j,nx,ny)][0]+out1[IDX(i,j,nx,ny)][1]*out2[IDX(i,j,nx,ny)][1];
				out[IDX(i,j,nx,ny)][1]=-out1[IDX(i,j,nx,ny)][0]*out2[IDX(i,j,nx,ny)][1]+out1[IDX(i,j,nx,ny)][1]*out2[IDX(i,j,nx,ny)][0];
			}


		/*  IFFT transform  */

		fftwf_execute(p3);
		fftwf_destroy_plan(p3); 



		for(i=0;i<nx;i++)
			for(j=0;j<ny;j++)
				corr[IDX(i,j,nx,ny)+k3*nx*ny]=in[IDX(i,j,nx,ny)][0]/(nx*ny);



	}   



	fftwf_free(in); fftwf_free(out);   
	fftwf_free(in1); fftwf_free(out1);
	fftwf_free(in2); fftwf_free(out2);


}
