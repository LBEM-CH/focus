

//get_mask(float *angles, int sx, int sz, int nx,int nx, flaot *mas)




#include <stdio.h>
#include <stdlib.h>
#include <fftw3.h>
#include <math.h>

int nx=420, sx=110, sz=40;

int rmax2=nx/2;


#define IRADA 0
#define IPAD  1
#define IRAD 1
#include "mask2D.c"
#include "trans3D.c"
#include "box_ft.c"
#include "ainterpo3dbig.c"
#include "ainterpo3d.c"
#include "extract2D.c"


 


main()
{	
	 FILE *output=fopen("test.dat","w");

	 float phas,   maxd, mind, wgt,aver;
	 float *mas=(float *)calloc(nx*nx,sizeof(float));

	 float *obj=(float *)calloc(nx*nx*nx, sizeof(float));
	 int i,j,k;
	 float *slice=(float *)calloc(nx*nx*nx*2,sizeof(float));
	 float *B=(float *)calloc(nx*nx*2,sizeof(float));

         fftwf_complex *in3,*out3, *in2, *out2;
         fftwf_plan  p2,p3; 
         in2=(fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*nx*nx);
         out2=(fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*nx*nx);      
         in3=(fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*nx*nx*nx);
         out3=(fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*nx*nx*nx); 
 

	  for(i=0;i<nx;i++)
	     for(j=0;j<nx;j++)
		for(k=0;k<nx;k++)
			obj[k+j*nx+i*nx*nx]=0.0;


	     for(k=nx/2-sz/2; k<nx/2+sz/2; k++)
	      for(i=0;i<nx;i++)
		for(j=0;j<nx;j++)
		    if((i-nx/2)*(i-nx/2)+(j-nx/2)*(j-nx/2)<sx*sx/4)
			obj[k+j*nx+i*nx*nx]=1;

   	      trans3D(nx,nx/2,nx/2,nx/2, obj); 


	      for(i=0;i<nx;i++)
	          for(j=0;j<nx;j++)
	          	for(k=0;k<nx;k++)
	          	{   	in3[k+j*nx+i*nx*nx][0]=obj[k+j*nx+i*nx*nx]*powf(-1.0,i+j+k);
                      		in3[k+j*nx+i*nx*nx][1]=0.0;   
	          	}
	        
              p3=fftwf_plan_dft_3d(nx,nx,nx, in3,out3,FFTW_FORWARD,FFTW_ESTIMATE);
              fftwf_execute(p3);
              fftwf_destroy_plan(p3);  

              for(i=0;i<nx;i++)
	        for(j=0;j<nx;j++)
	          for(k=0;k<nx;k++)
	          {	slice[k+j*nx+i*nx*nx]=out3[k+j*nx+i*nx*nx][0];
                    	slice[k+j*nx+i*nx*nx+nx*nx*nx]=out3[k+j*nx+i*nx*nx][1];   
	          }

for(i=0;i<nx;i++)
  for(j=0;j<nx;j++)
   {  	B[j+i*nx]=0.0;
    		B[j+i*nx+nx*nx]=0.0;
   } 
float angles[3];
angles[0]=30; angles[1]=45; angles[2]=80;
extract2D(B, angles, nx,nx,nx,slice);
	
for(i=0;i<nx;i++)
for(j=0;j<nx;j++)
{	out2[j+i*nx][0]=B[j+i*nx];
	out2[j+i*nx][1]=B[j+i*nx+nx*nx];
}


for(i=0;i<nx;i++)
 for(j=0;j<nx;j++)
 {     
	wgt=(sqrtf((i-nx/2)*(i-nx/2)*1.0+(j-nx/2)*(j-nx/2)*1.0));

				
          	       	  		 
          	         		 wgt=exp(-wgt*wgt/powf(rmax2*0.5,2.0));

	phas=-2*pi*((j+nx/2)*nx/2+(i+nx/2)*nx/2)/nx;
       out2[j+i*nx][0]=B[j+i*nx]*cos(phas)-B[j+i*nx+nx*nx]*sin(phas);
       out2[j+i*nx][1]=B[j+i*nx]*sin(phas)+B[j+i*nx+nx*nx]*cos(phas); 
	out2[j+i*nx][0]*=wgt;
	out2[j+i*nx][1]*=wgt;
    
 }

 
p3=fftwf_plan_dft_2d(nx,nx,out2,in2,FFTW_BACKWARD,FFTW_ESTIMATE);
fftwf_execute(p3);
fftwf_destroy_plan(p3);
     

  aver=0;
k=0;                     
for(i=0;i<nx;i++)
  for(j=0;j<nx;j++) 
  {  mas[j+i*nx]=in2[j+i*nx][0]*powf(-1.0,i+j); 
      if(powf(i-nx/2,2.0)+powf(j-nx/2,2.0)<powf(150,2))
	{	aver+=mas[j+i*nx];
		k++;
	}
   }

aver/=k;

// mask(100,100,nx,nx,mas);

 
maxd=-1.0e20; mind=-maxd;
for(i=0;i<nx;i++)
   for(j=0;j<nx;j++)
	if(mas[j+i*nx]>maxd) maxd=mas[j+i*nx];
	else if(mas[j+i*nx]<mind) mind=mas[j+i*nx];
for(i=0;i<nx;i++)
   for(j=0;j<nx;j++)
	 mas[j+i*nx]=(mas[j+i*nx]-mind)/(maxd-mind)*255;
 
 
/*
for(i=0;i<nx;i++)
   for(j=0;j<nx;j++)
	if(mas[j+i*nx]<aver)
		mas[j+i*nx]=0;
	else	mas[j+i*nx]=255;
*/

fprintf(output,"P2 %d %d \n  255 \n",nx,nx);
			
				for(i=0;i<nx;i++)
				{
					for(j=0;j<nx;j++)
						fprintf(output,"%d ",(int)mas[j+i*nx]);
					fprintf(output, "\n");
				}

}
