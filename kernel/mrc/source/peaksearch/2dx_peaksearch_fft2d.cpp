/*--------------------------------------------------------------------------------------------------
  Search the peaks in a PS file for the use of finding lattice of reciprocal space.
    
    May,2006  by  Xiangyan Zeng  in Stahlberg Lab
    

    
    Defined Parameters
              q_high:   cut of high pass filtering 
               q_low:   cut of low pass filtering
          mask_width:   horizental and vertical width of mask used to remove the artifact in PS file
      mask_radius_in:   inner radius of the mask
     mask_radius_out:   outer radius of the mask
              Npeaks:   Number of initial peaks obtained from original PS image
        Npeaks_final:   Number of peaks obtained from the average shifted PS image
     
     
     
     Output File
         2dx_peaksearch-phase.mrc  (phase of the original image) 
     
 ---------------------------------------------------------------------------------------------------*/    

#include "common.h" 
     
#define pi 3.1415926
#define q_high 0.05
#define q_low 0.25
#define Mask_width1 0.003
#define Mask_width2 0.002
#define Mask_radius_in1 0.01
#define Mask_radius_in2 0.05
#define Mask_radius_out1 0.49
#define Mask_radius_out2 0.49

using namespace std;

bool importWisdom()
{
  char str[80]; 
  strcpy(str, getenv("HOME"));
  strcat(str,"/.2dx/fftlib.wis");
  FILE *f = fopen(str,"r");
  if(f==NULL) return false;
  fftwf_import_wisdom_from_file(f);
  fclose(f);
  return true;
}

bool exportWisdom()
{
  char str[80]; 
  strcpy(str, getenv("HOME"));
  strcat(str,"/.2dx/fftlib.wis");
  FILE *f = fopen(str,"w");
  if(f==NULL) return false;
  fftwf_export_wisdom_to_file(f);
  fclose(f);
  return true;
}


int fft2d_small(char *filename, int Npeaks, int Npeaks_final, double inner_exclusion_radius, int mask_radius )
{  
 
 
   mrcImage *image = new mrcImage(filename);
   fftwf_complex *in, *out;
   fftwf_plan p1;
 
  
   int    sx,sy, nx,ny, i,j, k;
   int    *peak_x, *peak_y;
   int    *peak_x_final, *peak_y_final;
   float  *peak_value, *peak_value_final, Mask_radius_inner;
   float  *amp, *temp_phase, delta, *amp_small, mean, temp, max,min; 
    


   nx=image->width(); ny=image->height(); 
 
   cout<<": "<<endl;
   cout<<": Working on image of dimensions "<<nx<<","<<ny<<endl;
   cout<<": "<<endl;
   
   amp=(float *)malloc(sizeof(float)*nx*ny);
   temp_phase=(float *)malloc(sizeof(float)*nx*ny);
  
   in=(fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*nx*ny);
   out=( fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*nx*ny);
   

   peak_x=(int *)malloc(sizeof(int)*Npeaks);
   peak_y=(int *)malloc(sizeof(int)*Npeaks);
   peak_value=(float *)malloc(sizeof(float)*Npeaks);
   peak_x_final=(int *)malloc(sizeof(int)*Npeaks_final);
   peak_y_final=(int *)malloc(sizeof(int)*Npeaks_final);
   peak_value_final=(float *)malloc(sizeof(float)*Npeaks_final);   
   
   
   //importWisdom(); 
   p1=fftwf_plan_dft_2d(nx,ny,in,out,FFTW_FORWARD,FFTW_ESTIMATE);
 
   
   
/*  Initialize variables   */ 



    for(i=0; i<Npeaks; i++)
      {  peak_x[i]=0;  peak_y[i]=0;  peak_value[i]=0.0; }
           
     sx=mask_radius;  sy=mask_radius;
    for(i=0; i<Npeaks_final; i++)
      {  peak_x_final[i]=0;  peak_y_final[i]=0;  peak_value_final[i]=0.0;} 
     


/*   input images  */

    for(i=0;i<nx;i++) 
      for(j=0;j<ny;j++)
        {  
           if(image->mode()==0) temp=(float)((unsigned char*)image->rawData())[j*image->width()+i];
           else if(image->mode()==1)  temp=(float)((unsigned short*)image->rawData())[j*image->width()+i];  
	   else if(image->mode()==2) temp=(float)((float*)image->rawData())[j*image->width()+i];
           else {printf("Unsupported mode.\n Exiting..."); exit(1);}
           in[j+i*ny][0]=temp*pow(-1,(i+j)*1.0);
           in[j+i*ny][1]=0;
        }

  
/* FFT */
   

    fftwf_execute(p1);
    fftwf_destroy_plan(p1); 

/*  Get amplitude and phase of FFT image*/

    mean=0;
    for(i=0;i<nx;i++)
      for(j=0;j<ny;j++)
        {
           amp[j+i*ny]=sqrt(powf(out[j+i*ny][0],2.0)+powf(out[j+i*ny][1],2.0));
           mean=mean+amp[j+i*nx];

   /*  the following phase will be used in IFFTW of the real image */
           temp_phase[i+j*nx]=atan2(out[j+i*ny][1],out[j+i*ny][0]);
	   
	  
        }
   
 
       
     fftwf_free(in);fftwf_free(out);   



/*  Write the phase for later processing   */
     char *complexData = mrcImage::complexFromReal(nx,ny,2,(char*)temp_phase);
     mrcImage::mrcHeader *header = mrcImage::headerFromData(nx/2+1,ny,4,complexData);
   
     char fileName[] = "2dx_peaksearch-phase.mrc";
     mrcImage(header,complexData,fileName);
     cout<<fileName<<" written"<<endl;

     free(temp_phase);

/*   Cut a smaller image in the center area for finding lattice   */

         
     sx=mask_radius;  sy=mask_radius;

  printf("nx=%d ny=%d  sx=%d  sy=%d   %d----------------%d  \n",nx,ny,sx,sy,sx/2-sx/8, sx/2+sx/8);
  fflush(stdout);
  
     
     if(sx>2500 && sy>2500) 
     {  amp_small=(float *)malloc(sizeof(float)*sx/4*sy/4);

       for(i=nx/2-sx/8;i<(nx/2+sx/8); i++)
         for(j=ny/2-sy/8;j<(ny/2+sy/8); j++)
           {  amp_small[(j-(ny/2-sy/8))+(i-(nx/2-sx/8))*sy/4]=amp[j+i*ny];
  	   }
       sx=sx/4;  sy=sy/4; 
     }
     else if(sx>1025 && sy>1025)
     {  amp_small=(float *)malloc(sizeof(float)*sx/2*sy/2);
   
       for(i=nx/2-sx/4;i<(nx/2+sx/4); i++)
         for(j=ny/2-sy/4;j<(ny/2+sy/4); j++)
           {  amp_small[(j-(ny/2-sy/4))+(i-(nx/2-sx/4))*sy/2]=amp[j+i*ny];
  	   }
       sx=sx/2;  sy=sy/2; 
     }
     else
     { amp_small=(float *)malloc(sizeof(float)*sx*sy);
       for(i=0;i<sx*sy; i++)
       { 
         amp_small[i]=amp[i];
       }
     }
  
 
     printf("sx=%d  sy=%d  \n", sx,sy);
     fflush(stdout);
 


    free(amp);

 
     complexData = mrcImage::complexFromReal(sx,sy,2,(char*)amp_small);
     header = mrcImage::headerFromData(sx/2+1,sy,4,complexData);
   
     char fileName2[] = "2dx_peaksearch-non-masked_image.mrc";
     mrcImage(header,complexData,fileName2);
     cout<<"mrc file written: "<<fileName2<<endl;

        

/*   Mask the image */
     
     Mask_radius_inner = Mask_radius_in1;
     // This will allow to change Mask_radius_inner here in this file for test purposes
     cout<<"Given inner exclusion radius is "<<inner_exclusion_radius<<" (in FFT units [0...0.5])"<<endl;
     if (abs(inner_exclusion_radius-0.01)>0.0001) Mask_radius_inner = (float) inner_exclusion_radius;
     cout<<":Using an inner exclusion radius of "<<Mask_radius_inner<<" (in FFT units [0...0.5])"<<endl;
      

/*   Low-pass and high-pass to the small image  */


     printf("Low and highpass filter.\n"); 
     //low_high_pass(sx,sy,amp_small,q_low,q_high);


     min=1.0e40; max=-min;
     for(i=0;i<sx; i++)
      for(j=0;j<sy;j++)
        {
           if(min>amp_small[j+i*sy]) min=amp_small[j+i*sy];
           if(max<amp_small[j+i*sy]) max=amp_small[j+i*sy]; 
        }

 
     for(i=0;i<sx;i++)
        for(j=0;j<sy;j++)
	     amp_small[j+i*sy]=(amp_small[j+i*sy]-min)*10000.0/(max-min)+1;

 

/*   Mask the image */

   
       cout<<":Masking "<<Mask_radius_in2<<endl;
     
			 mask_image(sx,sy,amp_small,Mask_width1, Mask_radius_inner, Mask_radius_out1);
       //mask_image(sx,sy,amp_small,Mask_width2, Mask_radius_in2, Mask_radius_out2);
 

/*     Peak search  */
  
       cout<<":Peak_search "<<endl;

       peak_search(sx,sy,amp_small, Npeaks, peak_x,peak_y,peak_value);
 

/*  Shift  and get a average image  */  
  
      cout<<":shift "<<endl;
   
      shift(sx,sy, amp_small, Npeaks, peak_x,peak_y,peak_value);
     
     
 

 /*   Final peak search  in the average image */
           
     peak_search_final(sx,sy, amp_small, Npeaks_final,peak_x_final, peak_y_final, peak_value_final);
 
   
    
 //    cout<<"free amp_small"<<endl;
     free(amp_small);
     
     
 //    cout<<"free peak_x"<<endl;
     free(peak_x);
//     cout<<"free peak_y"<<endl;
     free(peak_y);
//     cout<<"free peak_value"<<endl;
     free(peak_value);

//     cout<<"free peak_x_final"<<endl;
     free(peak_x_final);
//     cout<<"free peak_y_final"<<endl;
     free(peak_y_final);
//     cout<<"free peak_value_final"<<endl;
     free(peak_value_final);
//     cout<<"all freed"<<endl;


    
     return 0;

 }



