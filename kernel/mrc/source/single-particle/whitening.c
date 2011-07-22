
/*
 *
 *  Created by Xiangyan Zeng on 1/1/2007
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */


#define MAXBIN   1000 
#define  pi      3.1415926

 


void whitening(int sx,int sy, float *Image1, float *ctf_para)
{    
        
     int *num;
     int   ISIZE=sx, Loop; 
     int i,j,k,m;
     
     float  WL,STEPR,THETATR,RAD,ANGLE,C1,C2,ANGDIF,CNTRST,DF,PHACON,ANGSPT,CCOS, AMPCON;
     float CS1, KV1;
     float  CS, KV,  DIFMID1, DIFMID2, ANGAST, DSTEP, XMAG;
 
     
     float  *ctf,*CHI;
     
     float  amp1;
   
     float CHI_max, CHI_min;
         
     float total_mean,total_num;
     float max1,min1,max2,min2, temp,mean,dev;
             
     float *mean_F, *mean_F2, *av_F, *devi;
     
     fftwf_complex *in, *out;
     fftwf_plan p1,p2;
   

	 CS=ctf_para[0];
	 KV=ctf_para[1];
	 DIFMID1=ctf_para[2];
	 DIFMID2=ctf_para[3];
	 ANGAST=ctf_para[4];
	 DSTEP=ctf_para[5];
	 XMAG=ctf_para[6];
     



    // mask(sx-10,sy-10, sx,sy, Image1);


     //     in=(fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*sx*sy);
     out=( fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*sx*sy);
     
     for(i=0;i<sx;i++)
        for(j=0;j<sy;j++)
               {    
                   out[j+i*sy][0]=(Image1[j+i*sy])*pow(-1.0,(i+j)*1.0); 
                   out[j+i*sy][1]=0;
               } 
 
 
     p1=fftwf_plan_dft_2d(sx,sy,out,out,FFTW_FORWARD,FFTW_ESTIMATE);        
     fftwf_execute(p1);
     fftwf_destroy_plan(p1);
  
       
     ctf=(float *)calloc(sx*sy,sizeof(float));
     CHI=(float *)calloc(sx*sy,sizeof(float));  
   
      
     av_F=(float *)calloc(MAXBIN,sizeof(float));
     
     mean_F=(float *)calloc(MAXBIN,sizeof(float));
     
     mean_F2=(float *)calloc(MAXBIN,sizeof(float));
     devi=(float *)calloc(MAXBIN,sizeof(float));
     num=(int *)calloc(MAXBIN,sizeof(int));
  
   
/*  CTF  */

     CS1=CS*(10000000);
     KV1=KV*1000;
     WL=12.3/sqrt(KV1+KV1*KV1/(1000000.0));

     STEPR=DSTEP*(10000)/XMAG;
     THETATR=WL/(STEPR*ISIZE); 

           
     AMPCON=0.07;
     PHACON=sqrt(1-AMPCON*AMPCON);
  
     for(i=0;i<sx;i++) 
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
		     {  ctf[j+i*sy]=-AMPCON;  // 1.0;
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
      
 
    
     CHI_max=-1.0e20;
     CHI_min=-CHI_max; 
     for(i=0;i<sx;i++)
          for(j=0;j<sy;j++)
             {  if(CHI_max<CHI[j+i*sy])  CHI_max=CHI[j+i*sy];
	           if(CHI_min>CHI[j+i*sy])  CHI_min=CHI[j+i*sy];
             }
  
     /*  Calculate the average spectrum */  
     mean=0;  
     for(i=0;i<sx;i++)
       for(j=0;j<sy;j++)
            mean+=Image1[j+i*sy];
     mean/=(sx*sy);
    
 
      /*  Get the whitening filter  */
 
     for(Loop=0;Loop<4;Loop++)
     {        
          for(i=0;i<MAXBIN;i++)
	     {  mean_F[i]=0.0; 
	        mean_F2[i]=0.0;
	        num[i]=0;
	      }	 
		 
  	     total_num=0; 
		for(i=0;i<sx;i++)
	      for(j=0;j<sy;j++)
	        {  
                amp1=pow(out[j+i*sy][0],2.0)+pow(out[j+i*sy][1],2.0);       
	           k=(int)(sqrtf(CHI[j+i*sy]-CHI_min)/sqrtf(CHI_max-CHI_min)*(MAXBIN-1));
		      if(Loop==0 || pow(amp1-av_F[k],2.0)<devi[k]) 
		      {
		          total_num+=1;
		   
		          mean_F[k]+=amp1;
		          mean_F2[k]+=powf(amp1,2.0);
		          num[k]++;
		       }		   
	        } 
			      
	      printf("Loop=%d  total_num=%f \n",Loop,total_num);
	      fflush(stdout);
		      
           for(i=0;i<MAXBIN;i++)
              if(num[i]>0)
	         {
	            av_F[i]=mean_F[i]/num[i];   
	            devi[i]=mean_F2[i]/num[i];
	            devi[i]=devi[i]-pow(av_F[i],2.0);
	        } 
      }	     
		      	 
        
     for(i=0;i<MAXBIN;i++)
	   mean_F[i]=sqrt(av_F[i]);
 
  
 
 
//     printf("::Get normalized images  \n");
 
        /*  Normalize the images */
 
     total_num=0;                        
     for(i=0;i<sx;i++)
	 for(j=0;j<sy;j++)
	 {         amp1=pow(out[j+i*sy][0],2.0)+pow(out[j+i*sy][1],2.0);
  
	           k=(int)(sqrtf(CHI[j+i*sy]-CHI_min)/sqrtf(CHI_max-CHI_min)*(MAXBIN-1));
           
	            if((k<MAXBIN/3 || amp1<av_F[k]*8) && mean_F[k]>0) 
		      {
		                
		            out[j+i*sy][0]=out[j+i*sy][0]/fabs(mean_F[k]+1.0e-30);
		            out[j+i*sy][1]=out[j+i*sy][1]/fabs(mean_F[k]+1.0e-30);  
		      }
	 	      else  
	 	      {     out[j+i*sy][0]=out[j+i*sy][0]/sqrtf(amp1+1.0e-30);
	  	             out[j+i*sy][1]=out[j+i*sy][1]/sqrtf(amp1+1.0e-30);
	 	      } 
		
/*
			if( (k>MAXBIN/6 && amp1>av_F[k]*6 )) 
			 {     out[j+i*sy][0]=out[j+i*sy][0]/sqrtf(amp1+1.0e-30);
	  	            out[j+i*sy][1]=out[j+i*sy][1]/sqrtf(amp1+1.0e-30);
	 	      }
		      else
			{
	        		     
		            out[j+i*sy][0]=out[j+i*sy][0]/(mean_F[k]+1.0e-30);
		            out[j+i*sy][1]=out[j+i*sy][1]/(mean_F[k]+1.0e-30);  
		      }
	 	     
*/	 	      
			 
		 
                  
         /*   Phase flip  is not used if local CTF is applied 
		      if(ctf[j+i*sy]<0)
	    	 {        out[j+i*sy][0]*=-1;  
                     out[j+i*sy][1]*=-1;  
		 }
		*/ 
		 	 		   
        }
	  
   
		
     free(ctf); 
     free(CHI);    
     free(av_F);
     free(mean_F);
     free(mean_F2);
     free(devi);
     free(num);	        
      
     
     p2=fftwf_plan_dft_2d(sx,sy,out,out,FFTW_BACKWARD,FFTW_ESTIMATE);
     fftwf_execute(p2);
     fftwf_destroy_plan(p2);
  
 
         
     for(i=0;i<sx;i++)
	   for(j=0;j<sy;j++)
	    Image1[j+i*sy]=out[j+i*sy][0]*pow(-1,(i+j)*1.0)/(sx*sy);  
 
	  

/*

	for(i=0;i<sx;i++)
		for(j=0;j<sy;j++)
		Image1[j+i*sy]=sqrt(powf(out[j+i*sy][0],2.0)+powf(out[j+i*sy][1],2.0));

*/         
   
//     fftwf_free(in);
     fftwf_free(out); 
    
      
       
	 

}
