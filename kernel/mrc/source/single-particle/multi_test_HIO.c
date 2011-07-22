//  HIO refine the 3D volume
 
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <fftw3.h>


int rmax2=45, IPAD=1;

#include "HIO.c"
#include "HIO1.c"
#include "ainterpo3dbig.c"
#include "extract2D.c"
#include "quadintp.c"
#include "rotate.c"
#include "Symmetrize3D.c"
main()
{	FILE *input[2], *output[3], *proj[3];

float angles[3];
angles[0]=0.0, angles[1]=0.0, angles[2]=0.0;



	int nx=140, i, j, k, Loop;
	float *refer=(float *)calloc(nx*nx*nx,sizeof(float)), maxd,mind;
	float *refer_temp=(float *)calloc(nx*nx*nx,sizeof(float));
 
	float  *demo=(float *)calloc(nx*nx,sizeof(float)), *B=(float *)calloc(nx*nx*2, sizeof(float))  ;
	float *slice=(float *)calloc(nx*nx*nx*2,sizeof(float));
	float max1=0, max2=0, min1=1.0e20, min2=1.0e20;
int *count=(int *)calloc(nx*nx*nx,sizeof(int));


	 fftwf_complex *in3,*out3;
	 fftwf_plan  p3; 
	 in3=(fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*nx*nx*nx);
      out3=(fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*nx*nx*nx);     

	input[0]=fopen("/home/xiangyan/GLPF/GLPF-clean/merge/SCRATCH/3dmodela_20.dat","r");
	input[1]=fopen("/home/xiangyan/GLPF/GLPF-clean/merge/SCRATCH/3dmodelb_20.dat","r");

	output[0]=fopen("/home/xiangyan/GLPF/GLPF-clean/merge/SCRATCH/3dmodela_HIO.dat","w");
	output[1]=fopen("/home/xiangyan/GLPF/GLPF-clean/merge/SCRATCH/3dmodela_HIO1.dat","w");
	output[2]=fopen("/home/xiangyan/GLPF/GLPF-clean/merge/SCRATCH/3dmodela_HIO2.dat","w");

 	proj[0]=fopen("/home/xiangyan/GLPF/GLPF-clean/merge/SCRATCH/proj1.pgm","w");
	proj[1]=fopen("/home/xiangyan/GLPF/GLPF-clean/merge/SCRATCH/proj2.pgm","w");
	proj[2]=fopen("/home/xiangyan/GLPF/GLPF-clean/merge/SCRATCH/proj3.pgm","w");

 
	fread(refer_temp, sizeof(float)*nx*nx*nx, 1, input[0]);
 

	fread(count, sizeof(int)*nx*nx*nx, 1, input[1]);



 

 
	for(i=0;i<nx;i++)
		for(j=0; j<nx; j++)
			for(k=0;k<nx;k++)
			refer[k+j*nx+i*nx*nx]=0.0;

	for(i=nx/2-40;i<nx/2+40;i++)
         for(j=nx/2-40;j<nx/2+40;j++) 
             for(k=nx/2-35;k<nx/2+35;k++)
			refer[k+j*nx+i*nx*nx]=refer_temp[k+j*nx+i*nx*nx];


  
     fwrite(refer, sizeof(float)*nx*nx*nx, 1, output[0]);
     fclose(output[0]);

	 
for(Loop=0; Loop<2; Loop++)
{

	//  get the FFT of centered object
	mind=1.0e20; maxd=-mind;
	for(i=0;i<nx;i++)
		for(j=0; j<nx; j++)
			for(k=0;k<nx;k++)
			{	
				if(refer[k+j*nx+i*nx*nx]<mind) mind=refer[k+j*nx+i*nx*nx];
				if(refer[k+j*nx+i*nx*nx]>maxd) maxd=refer[k+j*nx+i*nx*nx];
 			}
	printf("min=%f max=%f \n\n",mind,maxd);

 
 
	for(i=0;i<nx;i++)
		for(j=0; j<nx; j++)
			for(k=0;k<nx;k++)
			{	refer[k+j*nx+i*nx*nx]=(refer[k+j*nx+i*nx*nx]-mind) /(maxd-mind);
				in3[k+j*nx+i*nx*nx][0]=refer[k+j*nx+i*nx*nx]*powf(-1.0,i+j+k)/sqrt(nx*nx*nx);
				in3[k+j*nx+i*nx*nx][1]=0;
			}
  
	 p3=fftwf_plan_dft_3d(nx,nx,nx, in3,in3,FFTW_FORWARD,FFTW_ESTIMATE);
      fftwf_execute(p3);
      fftwf_destroy_plan(p3);

  
//  initialize the mssing FFT components using random values

	max1=0; max2=0;
	for(i=0;i<nx;i++)
	 for(j=0;j<nx;j++)
	   for(k=0;k<nx;k++)
	if(powf(in3[k+j*nx+i*nx*nx][0],2.0)+powf(in3[k+j*nx+i*nx*nx][1],2.0)<1.0e-4 &&  powf(i-nx/2,2.0)+powf(j-nx/2,2.0)+powf(k-nx/2,2.0)<rmax2*rmax2*0.49)
		{	 out3[k+j*nx+i*nx*nx][0]=(float)random()/RAND_MAX*50;
			 out3[k+j*nx+i*nx*nx][1]=(float)random()/RAND_MAX*50;

			if(powf(out3[k+j*nx+i*nx*nx][0],2.0)+powf( out3[k+j*nx+i*nx*nx][1],2.0)>max1)
			max1=powf(out3[k+j*nx+i*nx*nx][0],2.0)+powf( out3[k+j*nx+i*nx*nx][1],2.0);

		}
		else
		{	 out3[k+j*nx+i*nx*nx][0]= in3[k+j*nx+i*nx*nx][0];
			 out3[k+j*nx+i*nx*nx][1]= in3[k+j*nx+i*nx*nx][1];

			if(powf(out3[k+j*nx+i*nx*nx][0],2.0)+powf( out3[k+j*nx+i*nx*nx][1],2.0)>max2)
			max2=powf(out3[k+j*nx+i*nx*nx][0],2.0)+powf( out3[k+j*nx+i*nx*nx][1],2.0);
		}
	

	printf("Loop=%d  max1=%f  max2=%f \n",Loop, max1,max2);
	 

      p3=fftwf_plan_dft_3d(nx,nx,nx, out3,out3,FFTW_BACKWARD,FFTW_ESTIMATE);
      fftwf_execute(p3);
      fftwf_destroy_plan(p3);
    
         
      for(i=0;i<nx;i++)
           for(j=0;j<nx;j++) 
              for(k=0;k<nx;k++)
                 refer[k+j*nx+i*nx*nx]=out3[k+j*nx+i*nx*nx][0]*powf(-1.0,i+j+k)/sqrt(nx*nx*nx);	    
 
  

// HIO 

   
	for(i=0;i<50;i++)
	{
		 HIO(nx, in3, refer);
 //		 Symmetrize3D(nx,nx,nx,refer,count);
	}
 
/*
 
	for(i=0;i<30;i++)
	{
		 HIO1(nx, in3, refer);
 		 Symmetrize3D(nx,nx,nx,refer);
	}
 */
}	 
 
 
 
     for(k=0;k<45;k++)
		for(i=0;i<nx;i++)
			for(j=0;j<nx;j++)
			 refer[k+j*nx+i*nx*nx]=0;


 
     for(k=nx;k>nx-45;k--)
		for(i=0;i<nx;i++)
			for(j=0;j<nx;j++)
			 refer[k+j*nx+i*nx*nx]=0;
 

//	Symmetrize3D(nx,ny,nx,refer);

     fwrite(refer, sizeof(float)*nx*nx*nx, 1, output[2]);
     fclose(output[2]);



mind=1.0e20; maxd=-mind;
	for(i=0;i<nx;i++)
		for(j=0; j<nx; j++)
			for(k=0;k<nx;k++)
			{
				if(refer[k+j*nx+i*nx*nx]<mind) mind=refer[k+j*nx+i*nx*nx];
				if(refer[k+j*nx+i*nx*nx]>maxd) maxd=refer[k+j*nx+i*nx*nx];
 			}
printf("min=%f max=%f \n\n",mind,maxd);





for(i=0;i<nx;i++)
		for(j=0; j<nx; j++)
			for(k=0;k<nx;k++)
			{	in3[k+j*nx+i*nx*nx][0]=refer[k+j*nx+i*nx*nx]*powf(-1.0,i+j+k)/(nx*nx*nx);
				in3[k+j*nx+i*nx*nx][1]=0;
			}
p3=fftwf_plan_dft_3d(nx,nx,nx, in3,in3,FFTW_FORWARD,FFTW_ESTIMATE);
fftwf_execute(p3);
fftwf_destroy_plan(p3);

for(i=0;i<nx;i++)
	for(j=0; j<nx; j++)
		for(k=0;k<nx;k++)
		{	slice[k+j*nx+i*nx*nx]=in3[k+j*nx+i*nx*nx][0];
			slice[k+j*nx+i*nx*nx+nx*nx*nx]=in3[k+j*nx+i*nx*nx][1];
		}

 
extract2D(B, angles, nx,nx,nx,slice);
for(i=0;i<nx;i++)
   for(j=0;j<nx;j++)
	demo[j+i*nx]=sqrt(powf(B[j+i*nx],2.0)+powf(B[+i*nx+nx*nx],2.0));

  maxd=-1.0e20; mind=-maxd;
for(i=0;i<nx;i++)
   for(j=0;j<nx;j++)
	if(demo[j+i*nx]>maxd) maxd=demo[j+i*nx];
	else if(demo[j+i*nx]<mind) mind=demo[j+i*nx];
for(i=0;i<nx;i++)
   for(j=0;j<nx;j++)
	 demo[j+i*nx]=(demo[j+i*nx]-mind)/(maxd-mind)*255;

fprintf(proj[2],"P2 %d %d \n  255 \n",nx,nx);
demo[nx/2+nx/2*nx]=0;
for(i=0;i<nx;i++)
{
	for(j=0;j<nx;j++)
		fprintf(proj[2],"%d ",(int)demo[j+i*nx]);
	fprintf(proj[2], "\n");
}
fclose(proj[2]);

















}

