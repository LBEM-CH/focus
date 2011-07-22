/*
 *
 *  Created by Xiangyan Zeng on 1/1/2007
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */


#ifndef pi
#define pi  3.141592654
#endif


void envelop(int sx,int sy, float *refer, float A, float B, float *FSC, float *ctf_para)
{    
      int i,j,k,m;
     
      float WL,STEPR,THETATR,RAD,env;
      int   ISIZE=sx;
      float  CS, KV,  DIFMID1, DIFMID2, ANGAST, DSTEP, XMAG;      


      fftwf_complex *in, *out;
      fftwf_plan p1,p2;
     
      
      in=(fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*sx*sy);
      out=( fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*sx*sy);

	
	 CS=ctf_para[0];
	 KV=ctf_para[1];
	 DIFMID1=ctf_para[2];
	 DIFMID2=ctf_para[3];
	 ANGAST=ctf_para[4];
	 DSTEP=ctf_para[5];
	 XMAG=ctf_para[6];
     

      CS=CS*(10000000);
      KV=KV*1000;
      WL=12.3/sqrt(KV+KV*KV/(1000000.0));

      STEPR=DSTEP*(10000)/XMAG;
      THETATR=WL/(STEPR*ISIZE); 
     
      
      
/*   FFT transform */      
      for(i=0;i<sx;i++)
        for(j=0;j<sy;j++)
          {    
             in[j+i*sy][0]=refer[j+i*sy]*powf(-1,(i+j)*1.0); 
             in[j+i*sy][1]=0.0;
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
                     out[j+i*sy][0]=out[j+i*sy][0]*exp(env);
	             out[j+i*sy][1]=out[j+i*sy][1]*exp(env);

	       }
	      else
	       {
	          out[j+i*sy][0]=0;
	          out[j+i*sy][1]=0;
	       }
           
	  }  
	          

/*  IFFT transform  */

 
      fftwf_execute(p2);
      fftwf_destroy_plan(p2);
   
      
      
  
      for(i=0;i<sx;i++)
        for(j=0;j<sy;j++)
           refer[j+i*sy]=in[j+i*sy][0]*powf(-1,(i+j)*1.0);  
	    
  

      fftwf_free(in); fftwf_free(out);
      
       

}
