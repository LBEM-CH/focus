 
 
float spec_snr(int Num_images, int sx1,int sy1,  float *refer, float *Image, float *ssnr)
{     
      FILE *output;
       
      int   sx=mask_radius-6, sy=mask_radius-6, ISIZE=sx;
      float  max,mean,dev;
    
      fftwf_complex *in1, *out1, *in2, *out2;
      fftwf_plan p1,p2;
      int i,j,k,m, *num;
      float  *amp1, *amp2, *F1, *F2;  

      in1=(fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*sx*sy);
      out1=( fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*sx*sy);
      in2=(fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*sx*sy);
      out2=( fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*sx*sy);
      amp1=(float *)calloc(sx*sy,sizeof(float)); 
      amp2=(float *)calloc(sx*sy,sizeof(float)); 
     
      F1=(float *)calloc(sx,sizeof(float));
      F2=(float *)calloc(sy,sizeof(float));
      
      
    
      output=fopen("./SCRATCH/SSNR.dat","w");
      
 
 /*  Calculate the signal energy */     

      for(i=0;i<sx;i++)
        for(j=0;j<sy;j++)
	{	  
	   in1[j+i*sy][0]=refer[j+(sy1-sy)/2+(i+(sx1-sx)/2)*sy1]; 
           in1[j+i*sy][1]=0;    
	}

     
      for(i=0;i<sx;i++)
	for(j=0;j<sy;j++)
	   in1[j+i*sy][0]=(in1[j+i*sy][0])*pow(-1,(i+j)*1.0);  

      p1=fftwf_plan_dft_2d(sx,sy,in1,out1,FFTW_FORWARD,FFTW_ESTIMATE);
      fftwf_execute(p1);
      fftwf_destroy_plan(p1);

      for(i=0;i<sx;i++)
        for(j=0;j<sy;j++)
           amp1[j+i*sy]=powf(out1[j+i*sy][0],2)+powf(out1[j+i*sy][1],2);

      /*   Get the signal energy along the ring  */
       
      for(i=0;i<sx/2;i++)
         F1[i]=0;
           
      for(i=0;i<sx;i++)
         for(j=0;j<sy;j++) 
             {   m=(int)(sqrtf((i-sx/2)*(i-sx/2)*1.0+(j-sy/2)*(j-sy/2)*1.0));
                 if(m<sx/2)
                     F1[m]+=amp1[j+i*sy];            
             }

     
 
/*  Calculate the noise energy */
      
      for(i=0;i<sx;i++)
        for(j=0;j<sy;j++)
           amp2[j+i*sy]=0.0;

      for(k=0;k<Num_images;k++)
        {   	   

            for(i=0;i<sx;i++)
              for(j=0;j<sy;j++)
	       {	  
	           in2[j+i*sy][0]=Image[j+(sy1-sy)/2+(i+(sx1-sx)/2)*sy1+k*sx1*sy1]; 
                   in2[j+i*sy][1]=0;	    
	       }
  
            	 
            for(i=0;i<sx;i++)
 	       for(j=0;j<sy;j++)
	           in2[j+i*sy][0]=(in2[j+i*sy][0])*powf(-1,(i+j)*1.0); 

            p2=fftwf_plan_dft_2d(sx,sy,in2,out2,FFTW_FORWARD,FFTW_ESTIMATE);    
            fftwf_execute(p2);
            fftwf_destroy_plan(p2);
 

           /*   Get the noise energy    */
        
           for(i=0;i<sx;i++)
            for(j=0;j<sy;j++) 
             amp2[j+i*sy]+=(powf(out2[j+i*sy][0]-out1[j+i*sy][0],2)+powf(out2[j+i*sy][1]-out1[j+i*sy][1],2));            
           

      }


 /*   Get the noise energy along the ring  */
      for(i=0;i<sx/2;i++)
        F2[i]=0;
          
      for(i=0;i<sx;i++)
        for(j=0;j<sy;j++) 
             {   m=(int)(sqrtf((i-sx/2)*(i-sx/2)*1.0+(j-sy/2)*(j-sy/2)*1.0));
                 if(m<sx/2)
                   F2[m]+=amp2[j+i*sy];            
             } 


/*   output  the signal-to-noise ratio */

      for(i=0;i<sx/2;i++)
        {
           ssnr[i]=Num_images*F1[i]/(F2[i]/float(Num_images-1));
           if(ssnr[i]>=1)
             ssnr[i]=ssnr[i]-1;
           else ssnr[i]=0.0;
        }


       fprintf(output," i=%d  %f  %f %f   %f \n", 0, 0, F1[0], F2[0],ssnr[0]);
       for(i=1;i<sx/2;i++)
       fprintf(output," %d  %f  %f %f   %f \n", i, 1.0/(ISIZE*DSTEP*10000.0/(XMAG*i)), F1[i], F2[i],ssnr[i]);
	    

     fclose(output); 
        
     fftwf_free(in1);
     fftwf_free(out1);
     fftwf_free(in2); 
     fftwf_free(out2);

     free(amp1);
     free(amp2);
     free(F1);
     free(F2);


}
