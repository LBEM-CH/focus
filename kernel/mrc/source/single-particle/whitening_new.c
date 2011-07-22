

#define  pi      3.1415926

 


void whitening(int num_images, int sx1,int sy1, int sx, int sy, float *Image1, float *Image2)
{    
     FILE *output;
    
     int *num;
     int   ISIZE=sx, Loop; 
     int i,j,k,m,it;
     int *mask;
     
     
     double  WL,STEPR,THETATR,RAD,ANGLE,C1,C2,ANGDIF,CNTRST,DF,PHACON,ANGSPT,CCOS, AMPCON;
     double CS1, KV1; 
     
     float  *ctf,*CHI;
     
     
     float  *amp1;
     float CHI_max, CHI_min;
         
     double max1,min1,max2,min2, temp,mean,dev;
      
     output=fopen("ctf.dat","w");
 
     fftwf_complex *in, *out, *in_cp,*out_cp;
     fftwf_plan p1,p2;
     
     ctf=(float *)malloc(sizeof(float)*sx*sy);
     CHI=(float *)malloc(sizeof(float)*sx*sy); 
     amp1=(float *)malloc(sizeof(float)*sx*sy);
     
     in_cp=(fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*sx1*sy1);
     out_cp=( fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*sx1*sy1);
     in=(fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*sx*sy);
     out=( fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*sx*sy);
     
   
     fprintf(output,"P2 %d %d \n",sx,sy); 
     fprintf(output,"10001 \n");
 
 /*  CTF  */

     CS1=CS*(10000000);
     KV1=KV*1000;
     WL=12.3/sqrt(KV1+KV1*KV1/(1000000.0));

     STEPR=DSTEP*(10000)/XMAG;
     THETATR=WL/(STEPR*ISIZE); 

            
     AMPCON=0.07;
     PHACON=sqrt(1-AMPCON*AMPCON);
  
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
		 if(i==sx/2 && j==sy/2) 
		   {  ctf[j+i*sy]=1.0;
		      CHI[j+i*sy]=0;
		   } 
		 else if(DIFMID1==0.0 || DIFMID2==0.0 )
		     ctf[j+i*sy]=1.0;
		 else
		  {
      	             DF=0.5*(DIFMID1+DIFMID2+CCOS*(DIFMID1-DIFMID2));
      	             CHI[j+i*sy]=C1*DF+C2;
       	             ctf[j+i*sy]=-sin(CHI[j+i*sy])*PHACON-cos(CHI[j+i*sy])*AMPCON;
		     
		  }
	 
	      }  
 
      }
 
  
     CHI_max=-1.0e20;
     CHI_min=-CHI_max; 
     for(i=0;i<sx;i++)
          for(j=0;j<sy;j++)
             {  if(CHI_max<CHI[j+i*sy])  CHI_max=CHI[j+i*sy];
	        if(CHI_min>CHI[j+i*sy])  CHI_min=CHI[j+i*sy];
             }
 
  
     printf("CHI_min=%f  CHI_max=%f \n",CHI_min, CHI_max);
 
/*  Iteratively whiten each unit cell */  

     for(it=0;it<num_images;it++)
       { 
  
          /*  Calculate the average spectrum */  
  	 
	 	mean=0.0;
         for(i=0;i<sx;i++)
            for(j=0;j<sy;j++)
             mean+=Image1[j+i*sy1+it*sx1*sy1];
	
         mean/=(sx*sy);
	  
         for(i=0;i<sx1;i++)
            for(j=0;j<sy1;j++)
               {    
                   in_cp[j+i*sy1][0]=(Image1[j+i*sy1+it*sx1*sy1]-mean)*pow(-1.0,(i+j)*1.0); 
                   in_cp[j+i*sy1][1]=0;
               } 
 
 
         p1=fftwf_plan_dft_2d(sx1,sy1,in_cp,out_cp,FFTW_FORWARD,FFTW_ESTIMATE);
        
         fftwf_execute(p1);
         fftwf_destroy_plan(p1);
   
   
   /*
        if(it==77)
           { 
               
                 for(i=0;i<sx1;i++)
                   {   for(j=0;j<sy1;j++)
	                  fprintf(output,"%f ", sqrt(powf(out_cp[j+i*sy1][0],2.0)+powf(out_cp[j+i*sy1][1],2.0)));  
	           }
		
		fclose(output);
	    }
    */ 
       
   
   
   
   
        /*  Downsample in Fourier space */
      
         for(i=-sx/2;i<sx/2;i++)
            for(j=-sy/2;j<sy/2;j++)
	       {   out[j+sy/2+(i+sx/2)*sy][0]=out_cp[j+sy1/2+(i+sx1/2)*sy1][0];
                   out[j+sy/2+(i+sx/2)*sy][1]=out_cp[j+sy1/2+(i+sx1/2)*sy1][1];
      
               }
       
         for(i=0;i<sx;i++)
            for(j=0;j<sy;j++)   	 	   
	       amp1[j+i*sy]=pow(out[j+i*sy][0],2.0)+pow(out[j+i*sy][1],2.0);
 
 
        /*  normalize the images if applicable */
  
                                
         for(i=0;i<sx;i++)
	    for(j=0;j<sy;j++)
	       { 
	          if(do_whiten==1)
		  {         
                     out[j+i*sy][0]=(float)(out[j+i*sy][0]/sqrtf(amp1[j+i*sy]+1.0e-30)); 
	  	     out[j+i*sy][1]=(float)(out[j+i*sy][1]/sqrtf(amp1[j+i*sy]+1.0e-30));
		   
	 	    	 
                  if(ctf[j+i*sy]<0)
	     	     {  out[j+i*sy][0]*=-1;  
                        out[j+i*sy][1]*=-1;  
			
		     }
		  }
               }
        
          p2=fftwf_plan_dft_2d(sx,sy,out,in,FFTW_BACKWARD,FFTW_ESTIMATE);

          fftwf_execute(p2);
          fftwf_destroy_plan(p2);
            
          for(i=0;i<sx;i++)
	     for(j=0;j<sy;j++)
	        Image2[j+i*sy+it*sx*sy]=in[j+i*sy][0]*pow(-1,(i+j)*1.0)/(sx*sy);  
   	
  
          if(it==77)
           { 
                max2=-1.0e20;  min2=-max2;
                for(i=0;i<sx;i++)
                  for(j=0;j<sy;j++)
	          {   amp1[j+i*sy]=in[j+i*sy][0]; //  sqrt(powf(out[j+i*sy][0],2.0)+powf(out[j+i*sy][1],2.0)); 
	              if(max2<amp1[j+i*sy]) max2=amp1[j+i*sy];
	              if(min2>amp1[j+i*sy]) min2=amp1[j+i*sy];
	          }
  
                 printf("amp max=%f min=%f \n",max2,min2);
       
                 for(i=0;i<sx;i++)
                   {   for(j=0;j<sy;j++)
	                  fprintf(output,"%f ", (amp1[j+i*sy])); // -min2)/(max2-min2)*10001+1) ;
	               fprintf(output,"\n");
	           }
		
		fclose(output);
	    }
      
       
     
       }
   
     

     fftwf_free(in); fftwf_free(out); 
     fftwf_free(in_cp); fftwf_free(out_cp); 
     free(amp1); 
     free(CHI);
     free(ctf);  
     
      
       
	
      
       

}
