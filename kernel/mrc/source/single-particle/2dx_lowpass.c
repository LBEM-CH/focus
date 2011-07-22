/*
 *
 *  Created by Xiangyan Zeng on 1/1/2007
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */

 
void low_pass(int sx,int sy,float *amp, float q_l, int lp_m)
{  
   
   fftwf_complex *in, *out;
   fftwf_plan p1,p2;
   int i,j,k;
   float  *phase, mask_low, mask_high, delta_low, delta_high, tt, delta_low2; 

   
   phase=(float *)malloc(sizeof(float)*sx*sy);
   
  
  


   in=(fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*sx*sy);
   out=( fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*sx*sy);
 
   p1=fftwf_plan_dft_2d(sx,sy,in,out,FFTW_FORWARD,FFTW_ESTIMATE);
   p2=fftwf_plan_dft_2d(sx,sy,out,in,FFTW_BACKWARD,FFTW_ESTIMATE);



  

    for(i=0;i<sx;i++)
      for(j=0;j<sy;j++)
        {  
           in[j+i*sy][0]=amp[j+i*sy]*pow(-1,(i+j)*1.0); 
           in[j+i*sy][1]=0;
        }
 
   
   
    fftwf_execute(p1);


/*  Get amplitude and phase of FFT image*/
     for(i=0;i<sx;i++)
      for(j=0;j<sy;j++)
        {
           amp[j+i*sy]=sqrt(pow(out[j+i*sy][0],2)+pow(out[j+i*sy][1],2));
          
           phase[j+i*sy]=atan2(out[j+i*sy][1],out[j+i*sy][0]);
        }  



/*  Masking the frequency */
    
    
    delta_low=q_l*(sx+sy)/4;
    delta_low2=delta_low*delta_low;
    
    for(i=0;i<sx;i++)
       for(j=0;j<sy;j++)
           {   
              if(lp_m==1)
              { /* Gaussian low-pass filter. This also affects the low-resolution amplitude. */
                tt=-pow((double)(i-sx/2.0),2.0)-pow((double)(j-sy/2.0),2.0);
                mask_low=exp(tt/(2*delta_low*delta_low)); 
                amp[j+i*sy]=amp[j+i*sy]*mask_low;
              } 
              else if(lp_m==2)
              { /* for hard cut of frequency */
                // if(abs((double)(i-sx/2)*2)/sx<0.01 && abs((double)(i-sx/2)*2)/sx>0.1)
                tt=(i-sx/2)*(i-sx/2)+(j-sy/2)*(j-sy/2);
                if(tt>delta_low2)
                  amp[j+i*sy]=0;  
              }
            
	      out[j+i*sy][0]=amp[j+i*sy]*cos(phase[j+i*sy]);
              out[j+i*sy][1]=amp[j+i*sy]*sin(phase[j+i*sy]);                    
           }  

/*  IFFT transform  */

    fftwf_execute(p2);

    for(i=0;i<sx;i++)
       for(j=0;j<sy;j++)
         amp[j+i*sy]=in[j+i*sy][0]*pow(-1,(i+j)*1.0); 
      
     
    free(phase);   


}
