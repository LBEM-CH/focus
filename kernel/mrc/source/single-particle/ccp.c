
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <fftw3.h>
 
#ifndef pi
#define pi 3.141592654
#endif

float ccp(int nx, float *ang, int *shift, float *refer, float *proj, fftwf_plan p2_fw, fftwf_plan p2_bw)
{	
	
	float FFTWPA2=nx;
	int i,j,k ,m, k3, k4;	
	float wgt, pow_RT, phas, max, corr_image_total;
 	float *A2=(float *)calloc(nx*nx,sizeof(float)); 
	int *indx=(int *)malloc(sizeof(int)*nx*nx);
    	int *indy=(int *)malloc(sizeof(int)*nx*nx);
	float *B=(float *)calloc(nx*nx*2,sizeof(float));

 
	fftwf_complex *in,*out;
//    	fftwf_plan p1, p2; 
    	pthread_mutex_lock(&lock1);    	  
    	  in=(fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*nx*nx);
    	  out=(fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*nx*nx); 
        
//     	  p1=fftwf_plan_dft_2d(nx,nx,in,out, FFTW_FORWARD,FFTW_ESTIMATE);
//     	  p2=fftwf_plan_dft_2d(nx,nx,out,in,FFTW_BACKWARD,FFTW_ESTIMATE);
     	pthread_mutex_unlock(&lock1);     
 


	for(i=0;i<nx/2;i++)
      		for(j=0;j<nx;j++)
	    		indx[j+i*nx]=i;
	    
    	for(i=nx/2;i<nx;i++)
      		for(j=0;j<nx;j++)
         		indx[j+i*nx]=-(nx-i);    
	
    	for(i=0;i<nx;i++)
      		for(j=0;j<nx/2;j++)
         		indy[j+i*nx]=j;
	    
    	for(i=0;i<nx;i++)
      		for(j=nx/2;j<nx;j++)
  	    		indy[j+i*nx]=-(nx-j);
 
	// extract reference from the volume
	extract2D(B, ang, nx,nx,nx,refer);
 

	//  mask the reference in the real space
	for(i=0;i<nx;i++)
        	for(j=0;j<nx;j++)
        	{     	phas=-2*pi*((j+nx/2)*nx/2+(i+nx/2)*nx/2)/nx;
           		out[j+i*nx][0]=B[j+i*nx]*cos(phas)-B[j+i*nx+nx*nx]*sin(phas);
            		out[j+i*nx][1]=B[j+i*nx]*sin(phas)+B[j+i*nx+nx*nx]*cos(phas);   
                }
   
       	fftwf_execute_dft(p2_bw, out, in);
 
                            
        for(i=0;i<nx;i++)
        	for(j=0;j<nx;j++) 
        		A2[j+i*nx]=in[j+i*nx][0]*powf(-1.0,i+j)/FFTWPA2; 
                     
       	mask_refer(realcell_x1_common,realcell_y1_common,nx,nx,A2, base);

                             
        for(i=0;i<nx;i++)
     		for(j=0;j<nx;j++)
           	{        
           	        in[j+i*nx][0]=A2[j+i*nx]*powf(-1,i+j)/FFTWPA2;
            	        in[j+i*nx][1]=0;      
          	}    
 
        fftwf_execute_dft(p2_fw, in, out);
 
                             
        for(i=0;i<nx;i++)
        	for(j=0;j<nx;j++)
                {    	phas=-2*pi*((i+nx/2)*nx/2+(j+nx/2)*nx/2)/nx;
                        B[j+i*nx]=out[j+i*nx][0]*cos(phas)-out[j+i*nx][1]*sin(phas);
                        B[j+i*nx+nx*nx]=out[j+i*nx][0]*sin(phas)+out[j+i*nx][1]*cos(phas);
                }
         B[nx/2+nx/2*nx]=0;
         B[nx/2+nx/2*nx+nx*nx]=0;
  
        pow_RT=0;
	for(i=0;i<nx;i++)
		for(j=0;j<nx;j++)
	        {   
	                pow_RT+=powf(B[j+i*nx],2.0)+powf(B[j+i*nx+nx*nx],2.0);    
   
               }
     
         for(i=0;i<nx;i++)
	 	for(j=0;j<nx;j++)
	        {       
              		B[j+i*nx]=B[j+i*nx]*nx/sqrt(pow_RT);
	  		B[j+i*nx+nx*nx]=B[j+i*nx+nx*nx]*nx/sqrt(pow_RT);
          	}

 	
 	//  calculate the cross correlation of the reference and the projection
	for(i=0;i<nx;i++)
        	for(j=0;j<nx;j++)
        	{      m=(int)(sqrtf((i-nx/2)*(i-nx/2)*1.0+(j-nx/2)*(j-nx/2)*1.0));
 	
          		if(m>=rmax1 && m<rmax2)
          	        { 
				wgt=exp(-m*m/powf(rmax2*0.8,2.0));
          			out[j+i*nx][0]=proj[j+i*nx]*B[j+i*nx]+proj[j+i*nx+nx*nx]*B[j+i*nx+nx*nx];
	      			out[j+i*nx][1]= -proj[j+i*nx]*B[j+i*nx+nx*nx]+proj[j+i*nx+nx*nx]*B[j+i*nx]; 

				out[j+i*nx][0]*=wgt;
				out[j+i*nx][1]*=wgt;		
     			} 
	   		else
	     		{	out[j+i*nx][0]=0.0;
	     			out[j+i*nx][1]=0.0;
	      		}
	    	}	 

 
        fftwf_execute_dft(p2_bw, out, in);
 

	max=-1.0e20;
	for(k3=0; k3<nx;  k3++)
        	for(k4=0; k4<nx; k4++)
                {   
                    	i=indx[k4+k3*nx];
                    	j=indy[k4+k3*nx];
        
                    	if(abs(i)<=5  && abs(j)<=5 )
                    	{	
                         	corr_image_total=in[k4+k3*nx][0]*powf(-1.0,k3+k4)/(nx); 
				if(max< corr_image_total)
				{	shift[0]=-i;
					shift[1]=-j;
					max=corr_image_total;
				}
	              	}	
	         }


	 free(indx);
	 free(indy);
	 free(B);
	 free(A2);

 
	 pthread_mutex_lock(&lock4);     
                fftwf_free(in); 
                fftwf_free(out);
//                fftwf_destroy_plan(p1);
//                fftwf_destroy_plan(p2); 
         pthread_mutex_unlock(&lock4);     
      
 	
	return(max);
}
