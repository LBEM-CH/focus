//  HIO refine the 3D volume
 
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <fftw3.h>


int rmax2=60, IPAD=1;

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



	int nx=140, i, j, k;
	float *refer=(float *)calloc(nx*nx*nx,sizeof(float)), maxd,mind;
	float *refer_temp=(float *)calloc(nx*nx*nx,sizeof(float));
 
	float  *demo=(float *)calloc(nx*nx,sizeof(float)), *B=(float *)calloc(nx*nx*2, sizeof(float))  ;
	float *slice=(float *)calloc(nx*nx*nx*2,sizeof(float));
	float max1=0, max2=0, min1=1.0e20, min2=1.0e20;
 


	 fftwf_complex *in3,*out3;
	 fftwf_plan  p3; 
	 in3=(fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*nx*nx*nx);
      out3=(fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*nx*nx*nx);     


	input[0]=fopen("/home/xiangyan/mrcImages/GLPF/GLPF-clean/merge/SCRATCH/3dmodela_37.dat","r");
 
	output[0]=fopen("/home/xiangyan/mrcImages/GLPF/GLPF-clean/merge/SCRATCH/3dmodela_HIO.dat","w");
	output[1]=fopen("/home/xiangyan/mrcImages/GLPF/GLPF-clean/merge/SCRATCH/3dmodela_HIO1.dat","w");
	output[2]=fopen("/home/xiangyan/mrcImages/GLPF/GLPF-clean/merge/SCRATCH/3dmodela_HIO2.dat","w");

 	proj[0]=fopen("/home/xiangyan/mrcImages/GLPF/GLPF-clean/merge/SCRATCH/proj1.pgm","w");
	proj[1]=fopen("/home/xiangyan/mrcImages/GLPF/GLPF-clean/merge/SCRATCH/proj2.pgm","w");
	proj[2]=fopen("/home/xiangyan/mrcImages/GLPF/GLPF-clean/merge/SCRATCH/proj3.pgm","w");

 
	fread(refer_temp, sizeof(float)*nx*nx*nx, 1, input[0]);
  
 
	for(i=0;i<nx;i++)
		for(j=0; j<nx; j++)
			for(k=0;k<nx;k++)
			refer[k+j*nx+i*nx*nx]=refer_temp[k+j*nx+i*nx*nx];
 
 

  
     fwrite(refer, sizeof(float)*nx*nx*nx, 1, output[0]);
     fclose(output[0]);

	 

	//  get the FFT of centered object
	mind=1.0e20; maxd=-mind;
	for(i=0;i<nx;i++)
		for(j=0; j<nx; j++)
			for(k=0;k<nx;k++)
			{
				if(refer[k+j*nx+i*nx*nx]<mind) mind=refer[k+j*nx+i*nx*nx];
				if(refer[k+j*nx+i*nx*nx]>maxd) maxd=refer[k+j*nx+i*nx*nx];
 			}
	printf("min=%e max=%e \n\n",mind,maxd);

 
 
	for(i=0;i<nx;i++)
		for(j=0; j<nx; j++)
			for(k=0;k<nx;k++)
			{	refer[k+j*nx+i*nx*nx]*=1;
 
				in3[k+j*nx+i*nx*nx][0]=refer[k+j*nx+i*nx*nx]*powf(-1.0,i+j+k)/sqrt(nx*nx*nx);
				in3[k+j*nx+i*nx*nx][1]=0;
			}
  
	 p3=fftwf_plan_dft_3d(nx,nx,nx, in3,in3,FFTW_FORWARD,FFTW_ESTIMATE);
      fftwf_execute(p3);
      fftwf_destroy_plan(p3);


int im,jm,km;
for(i=0;i<nx;i++)
for(j=0;j<nx;j++)
for(k=0;k<nx;k++)
if(max1<powf(in3[k+j*nx+i*nx*nx][0],2.0)+powf(in3[k+j*nx+i*nx*nx][1],2.0))
{ max1=powf(in3[k+j*nx+i*nx*nx][0],2.0)+powf(in3[k+j*nx+i*nx*nx][1],2.0);
//	im=i; jm=j; km=k;
}
else if(min1>powf(in3[k+j*nx+i*nx*nx][0],2.0)+powf(in3[k+j*nx+i*nx*nx][1],2.0))
{  min1=powf(in3[k+j*nx+i*nx*nx][0],2.0)+powf(in3[k+j*nx+i*nx*nx][1],2.0);
		im=i; jm=j; km=k;
}

	printf("Entire image min1=%f  max1=%f  im=%d   jm=%d  km=%d        center is %f\n",min1,max1, im, jm,km,powf(in3[nx/2+nx/2*nx+nx/2*nx*nx][0],2.0)+powf(in3[nx/2+nx/2*nx+nx/2*nx*nx][1],2.0));



 
for(i=nx/2-1;i<=nx/2+1;i++)
{ printf("\n");
for(j=nx/2-1;j<=nx/2+1;j++)
{printf("\n");
for(k=nx/2-1;k<=nx/2+1;k++)
printf("%e %e     ", in3[k+j*nx+i*nx*nx][0], in3[k+j*nx+i*nx*nx][1]);
}} 
printf("\n");





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

	fprintf(proj[0],"P2 %d %d \n  255 \n",nx,nx);
	demo[nx/2+nx/2*nx]=0;
	for(i=0;i<nx;i++)
	{
		for(j=0;j<nx;j++)
			fprintf(proj[0],"%d ",(int)demo[j+i*nx]);
		fprintf(proj[0], "\n");
	}
	fclose(proj[0]);


















//  initialize the mssing FFT components using random values

	max1=0; max2=0;
	for(i=0;i<nx;i++)
	 for(j=0;j<nx;j++)
	   for(k=0;k<nx;k++)
	if(powf(in3[k+j*nx+i*nx*nx][0],2.0)+powf(in3[k+j*nx+i*nx*nx][1],2.0)<1.0e-4 &&  powf(i-nx/2,2.0)+powf(j-nx/2,2.0)+powf(k-nx/2,2.0)<rmax2*rmax2*0.49)
		{	 out3[k+j*nx+i*nx*nx][0]=  in3[k+j*nx+i*nx*nx][0]; // + (float)random()/RAND_MAX*100000;
			 out3[k+j*nx+i*nx*nx][1]= in3[k+j*nx+i*nx*nx][1]; //+(float)random()/RAND_MAX*100000;

			if(powf(out3[k+j*nx+i*nx*nx][0],2.0)+powf( out3[k+j*nx+i*nx*nx][1],2.0)>max1)
			max1=powf(out3[k+j*nx+i*nx*nx][0],2.0)+powf( out3[k+j*nx+i*nx*nx][1],2.0);

		}
		else
		{	 out3[k+j*nx+i*nx*nx][0]= in3[k+j*nx+i*nx*nx][0];
			 out3[k+j*nx+i*nx*nx][1]= in3[k+j*nx+i*nx*nx][1];

			if(powf(out3[k+j*nx+i*nx*nx][0],2.0)+powf( out3[k+j*nx+i*nx*nx][1],2.0)>max2)
			max2=powf(out3[k+j*nx+i*nx*nx][0],2.0)+powf( out3[k+j*nx+i*nx*nx][1],2.0);
		}
	

	printf("max1=%e  max2=%e \n",max1,max2);
	 

      p3=fftwf_plan_dft_3d(nx,nx,nx, out3,out3,FFTW_BACKWARD,FFTW_ESTIMATE);
      fftwf_execute(p3);
      fftwf_destroy_plan(p3);
    
         
      for(i=0;i<nx;i++)
           for(j=0;j<nx;j++) 
              for(k=0;k<nx;k++)
                 refer[k+j*nx+i*nx*nx]=out3[k+j*nx+i*nx*nx][0]*powf(-1.0,i+j+k)/sqrt(nx*nx*nx);	    
 
 
     fwrite(refer, sizeof(float)*nx*nx*nx, 1, output[1]);
     fclose(output[1]);




for(i=0;i<nx;i++)
	for(j=0; j<nx; j++)
		for(k=0;k<nx;k++)
		{	slice[k+j*nx+i*nx*nx]=out3[k+j*nx+i*nx*nx][0];
			slice[k+j*nx+i*nx*nx+nx*nx*nx]=out3[k+j*nx+i*nx*nx][1];
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

fprintf(proj[1],"P2 %d %d \n  255 \n",nx,nx);
demo[nx/2+nx/2*nx]=0;
for(i=0;i<nx;i++)
{
	for(j=0;j<nx;j++)
		fprintf(proj[1],"%d ",(int)demo[j+i*nx]);
	fprintf(proj[1], "\n");
}
fclose(proj[1]);












   



// HIO 

  int m;
	for(m=0;m<30;m++)
	{
		 HIO(nx, in3, refer);

printf("m=%d ",m);
 //		 Symmetrize3D(nx,nx,nx,refer,count);
 
	}
 
 

/*
 
	for(i=0;i<30;i++)
	{
		 HIO1(nx, in3, refer);
 		 Symmetrize3D(nx,nx,nx,refer);
	}
 */
	 
 
 
 /*
     for(k=0;k<40;k++)
		for(i=0;i<nx;i++)
			for(j=0;j<nx;j++)
			 refer[k+j*nx+i*nx*nx]=0;


 
     for(k=nx;k>nx-40;k--)
		for(i=0;i<nx;i++)
			for(j=0;j<nx;j++)
			 refer[k+j*nx+i*nx*nx]=0;
 */



 /* 3D median filtering
		float *medi=(float *)calloc(27,sizeof(float));
int k3, k4, m,n;
		float tmp; 
		for(i=0;i<nx;i++)
			for(j=0;j<nx;j++)
				for(k=0;k<nx;k++)
					if(i-1>0 && i+1<nx && j-1>0 && j+1<nx && k-1>0 && k+1<nx)
					{	n=0;
						 
						for(k3=i-1;k3<=i+1; k3++)
							for(k4=j-1;k4<=j+1;k4++)
								for(m=k-1;m<=k+1; m++)
								{	medi[n]=refer[m+k4*nx+k3*nx*nx];
									n++;
								}

						for(k3=0;  k3<26;  k3++)
			   				for(k4=26;  k4>k3;  k4--)
								if(medi[k4]>medi[k4-1])
								{	 
									tmp=medi[k4-1];
									medi[k4-1]=medi[k4];
									medi[k4]=tmp;
								}
						 
					//	for(k3=10;k3<19;k3++)
							refer_temp[k+j*nx+i*nx*nx]=medi[13];
					}
					else refer_temp[k+j*nx+i*nx*nx]=0;

		 
		free(medi);
  for(i=0;i<nx;i++)
		for(j=0; j<nx; j++)
			for(k=0;k<nx;k++)
			refer[k+j*nx+i*nx*nx]=refer_temp[k+j*nx+i*nx*nx];

*/






//	Symmetrize3D(nx,nx,nx,refer);

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

