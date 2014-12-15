/*
 *
 *  Created by Xiangyan Zeng on 1/1/2007
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */


#include <common.h>
#include "interpolate.c"


void amp_ph(int nx, int ny, int sx, float *real_lat, float *image1,float *image2, FILE *results) 
{  
      FILE *output, *test;

      int i,j;
      float *amp, *phase, *temp_image;
 
      fftwf_complex *in, *out;
      fftwf_plan p;

      in=(fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*sx*sx);
      out=( fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*sx*sx);  
  
      
      amp=(float *)calloc(sx*sx,sizeof(float));
      phase=(float *)calloc(sx*sx,sizeof(float));
      temp_image=(float *)calloc(sx*sx,sizeof(float));
   
      char fileName3[] = "APH/ML_result.aph";
      char fileName4[] = "SCRATCH/ML_unitcell.pgm";
  
      output=fopen(fileName3,"w");
      test=fopen(fileName4,"w");

 

      fprintf(results,"# IMAGE-IMPORTANT: %s <APH: ML result>\n",fileName3);
 

      fprintf(results,"# IMAGE-IMPORTANT: %s <PGM: ML result, UnitCell>\n",fileName4);

 
 
 
      reinterpolat(nx,ny,real_lat,image1,image2,sx);

 
 
/*   Output reinterpolated unit cell into MRC format file */

      float min, max;
      min=image2[0];
      max=min;
      for(i=0;i<sx;i++)
       for(j=0;j<sx;j++)
        { 
           if(max<image2[IDX(i,j,sx,sx)]) max=image2[IDX(i,j,sx,sx)];
           else if(min>image2[IDX(i,j,sx,sx)]) min=image2[IDX(i,j,sx,sx)]; 
        }

      fprintf(test,"P2 %d %d \n 256 \n",sx,sx);
      for(i=0;i<sx;i++)
      {  for(j=0;j<sx;j++)
            {
               fprintf(test,"%d ",(int)((image2[IDX(i,j,sx,sx)]-min)*255/(max-min)));
            }
         fprintf(test,"\n");
       }    
      fclose(test);


/* Calculate FFT of unit cell */

      for(i=0;i<sx;i++)
        for(j=0;j<sx;j++)
           {  
              in[IDX(i,j,sx,sx)][0]=image2[IDX(i,j,sx,sx)]*powf(-1.0,(float)(i+j));
              in[IDX(i,j,sx,sx)][1]=0.0;
           }


      p=fftwf_plan_dft_2d(sx,sx,in,out,FFTW_FORWARD,FFTW_ESTIMATE);
      
      fftwf_execute(p);
      fftwf_destroy_plan(p);



/*  Get amplitude and phase of FFT image   */
      int IQ=1;
      float rback=0.1;
      float ctf=0.999;
      float tmp_phase;
      for(i=0;i<sx;i++)
        for(j=0;j<sx;j++)
         {
           amp[IDX(i,j,sx,sx)]=sqrtf(pow(out[IDX(i,j,sx,sx)][0],2)+pow(out[IDX(i,j,sx,sx)][1],2));
           phase[IDX(i,j,sx,sx)]=atan2(out[IDX(i,j,sx,sx)][1], out[IDX(i,j,sx,sx)][0]); 	   
           tmp_phase=phase[IDX(i,j,sx,sx)]*180.0/3.141592654; 
           tmp_phase+=180.0*i;
           tmp_phase+=180.0*j;
           if(SYM==1)tmp_phase+=180.0*i;
           while(tmp_phase>360.0)tmp_phase-=360.0;
           if(tmp_phase<0)tmp_phase+=360.0;
           fprintf(output," %4d %4d %11.1e %11.1f %3d %11.1f %11.3f\n",i-sx/2,j-sx/2,amp[IDX(i,j,sx,sx)],tmp_phase,IQ,rback,ctf); 
         } 


      fclose(output);
      
      fftwf_free(in);
      fftwf_free(out);

      free(amp);
      free(phase);
      free(temp_image); 


}
