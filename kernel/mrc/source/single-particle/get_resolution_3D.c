/*
 *
 *  Created by Xiangyan Zeng on 1/1/2007
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */



#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <fftw3.h>
  

float  DSTEP=7.0, XMAG=70000;

#ifndef pi
#define pi 3.141592654
#endif
 
//float resolution3D(int sx1,int sy1,float *ref1, float *ref2,float *FRC, float DSTEP, float XMAG)

main()
{     
     FILE *input[2], *output;    
     int  sx=120;
 	float  *ref1, *ref2, *FSC;
    
     float   mean;
      
     int   ISIZE=sx; 
    
     fftwf_complex *in1, *out1, *in2, *out2;
     fftwf_plan p1,p2;
     int k2,k3,k4,i,j,k,m,im,jm,km,*num;
     float  *amp1, *amp2, *corr,  *F1, *F2, *F12,  max12,min, max1, max2,max3;  
     
     float Step_angle=1;
     int Num_angles=30;
 
	ref1=(float *)calloc(sx*sx*sx,sizeof(float));
	ref2=(float *)calloc(sx*sx*sx,sizeof(float));

	FSC=(float *)calloc(sx,sizeof(float));

     in1=(fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*sx*sx*sx);
     out1=( fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*sx*sx*sx);
     in2=(fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*sx*sx*sx);
     out2=( fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*sx*sx*sx);
     amp1=(float *)calloc(sx*sx*sx,sizeof(float));  
     amp2=(float *)calloc(sx*sx*sx,sizeof(float));
      
     corr=(float *)calloc(sx*sx*sx,sizeof(float));
     
     F1=(float *)calloc(sx,sizeof(float));
     F2=(float *)calloc(sx,sizeof(float));
     F12=(float *)calloc(sx,sizeof(float)); 
     
     num=(int *)calloc(sx,sizeof(int));
      
     
     input[0]=fopen("/home/xiangyan/GLPF/GLPF-clean/merge/SCRATCH/CC_results_6/3dmodela1_07.dat","r");
	input[1]=fopen("/home/xiangyan/GLPF/GLPF-clean/merge/SCRATCH/CC_results_6/3dmodela2_07.dat","r");
     output=fopen("/home/xiangyan/GLPF/GLPF-clean/merge/SCRATCH/FSC.dat","w");
     
 
 	fread(ref1, sizeof(float)*sx*sx*sx, 1, input[0]);
	fread(ref2, sizeof(float)*sx*sx*sx, 1, input[1]);
 
	 
 
      for(i=0;i<sx;i++)
         for(j=0;j<sx;j++)
		for(k=0;k<sx;k++)
          {		 
	    		 in1[k+j*sx+i*sx*sx][0]=ref1[k+j*sx+i*sx*sx] ;  	 
	     	 in2[k+j*sx+i*sx*sx][0]=ref2[k+j*sx+i*sx*sx];  

			 in1[k+j*sx+i*sx*sx][1]=0; 
            	 in2[k+j*sx+i*sx*sx][1]=0;
            	 
           }  
      
           
      
/*  Normalize the references */      
      
      
      mean=0;
      for(i=0;i<sx;i++)
         for(j=0;j<sx;j++)
		for(k=0;k<sx;k++)
	     	mean+=in1[k+j*sx+i*sx*sx][0];
      mean/=(sx*sx*sx);
      for(i=0;i<sx;i++)
         for(j=0;j<sx;j++)
		for(k=0;k<sx;k++)
		    in1[k+j*sx+i*sx*sx][0]=(in1[k+j*sx+i*sx*sx][0]-mean)*powf(-1,(i+j+k)*1.0)/(sx*sx*sx);
      
      mean=0;
      for(i=0;i<sx;i++)
         for(j=0;j<sx;j++)
		for(k=0;k<sx;k++)
	     	mean+=in2[k+j*sx+i*sx*sx][0];
      mean/=(sx*sx*sx);
      for(i=0;i<sx;i++)
         for(j=0;j<sx;j++)
		for(k=0;k<sx;k++)
		    in2[k+j*sx+i*sx*sx][0]=(in2[k+j*sx+i*sx*sx][0]-mean)*powf(-1,(i+j+k)*1.0)/(sx*sx*sx);
     
      
      p1=fftwf_plan_dft_3d(sx,sx,sx, in1,out1,FFTW_FORWARD,FFTW_ESTIMATE);
 	 p2=fftwf_plan_dft_3d(sx,sx,sx, in2,out2,FFTW_FORWARD,FFTW_ESTIMATE);
    
      fftwf_execute(p1);
      fftwf_destroy_plan(p1);
      fftwf_execute(p2);
      fftwf_destroy_plan(p2);



/*  Get amplitude and phase of FFT image   */
      for(i=0;i<sx;i++)
        for(j=0;j<sx;j++)
		for(k=0;k<sx;k++)
         {
	            amp1[k+j*sx+i*sx*sx]=(pow(out1[k+j*sx+i*sx*sx][0],2)+pow(out1[k+j*sx+i*sx*sx][1],2));

			  amp2[k+j*sx+i*sx*sx]=(pow(out2[k+j*sx+i*sx*sx][0],2)+pow(out2[k+j*sx+i*sx*sx][1],2));
         	   
 
         
           	  corr[k+j*sx+i*sx*sx]=(out1[k+j*sx+i*sx*sx][0]*out2[k+j*sx+i*sx*sx][0]+out1[k+j*sx+i*sx*sx][1]*out2[k+j*sx+i*sx*sx][1]);
        
         } 
	    

/*   Get  Fourier Ring Correlation  */

      for(i=0;i<sx; i++)
         {  F1[i]=0;
             F2[i]=0;
             F12[i]=0;
             num[i]=0;
         }

 
  
      for(i=0;i<sx;i++)
           for(j=0;j<sx;j++) 
		  for(k=0; k<sx;k++)	
             {  
                  m=(int)(sqrtf(powf(i-sx/2,2.0)+powf(j-sx/2,2.0)+powf(k-sx/2,2.0)));

                  F1[m]+=(amp1[k+j*sx+i*sx*sx]);
		  	   F2[m]+=(amp2[k+j*sx+i*sx*sx]);
		  	   F12[m]+=corr[k+j*sx+i*sx*sx];
		        num[m]++;
 
              }

 

/*
 
        int rad;
        int ix,iy;
      
        for(rad=1;rad<sx/2;rad++)
         {   for(i=0;i<(int)(2*pi*rad);i++)
               {   ix=(int)(rad*cos((float)i/(float)rad)+sx/2);  
                    iy=(int)(rad*sin((float)i/(float)rad)+sy/2); 
                   
                    F1[rad]+=powf(amp1[iy+ix*sy],2.0);
		          F2[rad]+=powf(amp2[iy+ix*sy],2.0);
		          F12[rad]+=corr[iy+ix*sy];
		          num[rad]++;
                  
               }
         }
  */
 


	  FSC[0]=1;
       for(i=1;i<sx/2;i++)   
	  {   FSC[i]=F12[i]/sqrtf(F1[i]*F2[i]);
			printf("%d  %f  %f  %f   %f \n", i, F12[i], F1[i], F2[i], FSC[i]);
	  }
    
	           
        
 
/*   output  the result  */

      m=sx/2;
      max12=-1.0e20;   
      for(i=0;i<sx/2;i++)
          if(FSC[i]>max12) max12=FSC[i];
        
  
      max1=-1.0e20;   
      for(i=0;i<sx/2;i++)
        if(F1[i]>max1) max1=F1[i];
        
 
      for(i=0;i<sx/2;i++)
            fprintf(output,"   %f     %f \n",   1.0/((float)(sx*DSTEP*10000)/(float)(XMAG*(i)*1)), FSC[i]);
	 
    

     fclose(input[0]); 
     fclose(input[1]);
	fclose(output);
   
     fftwf_free(in1); fftwf_free(out1);
     fftwf_free(in2); fftwf_free(out2);  
 


     free(amp1);
     free(amp2);
     free(ref1);
     free(ref2);
     free(corr);
     free(F1);
     free(F2);
     free(num);  
       

}
