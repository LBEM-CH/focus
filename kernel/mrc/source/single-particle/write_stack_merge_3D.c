/*   Generate and  write a stack
*   A stack of unit cells are extracted from a single crystal image or patch
*   input:  
*       Numstack:  number of stack
*       dirname:    directory name of the processed crystal
* 
*   output:
*        two binary files are written to the directory:
*        filename_whit.binary: whitened stacks 
*        filename_ctf.binary:  ctf corrected stacks
*/     


float  CS, KV, RESMIN, RESMAX, DIFMID1, DIFMID2, ANGAST, DSTEP, XMAG,phaori[2],lattice[4],defocus[3], TLTAXIS, TLTANG, TLTAXA,TAXA,TANGL, ctf_para[7];



#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fftw3.h>


#include "ctf_local.c"
#include "mask2D.c"
#include "ctf.c"
#include "whitening.c"
#include "realspace_lattice.c"
#include "reciprocal.c"
#include "get_units_3D.c"
#include "get_units_merge_3D.c"
#include "normalize_image.c"
#include "2dx_lowpass.c"
#include "align.c"
#include "Symmetrize.c"
using namespace std;
 



int write_stack_merge(int Numstack, char *dirname)
{
     
     
	float  *unbend_image1,*unbend_image2,  *unbend_image3, *Image1, *Image2, *Image3;
     float  *peak_x, *peak_y, *peak_z;
     int    all_peak_counter, sta;
     int    sx,sy, i,j, k,m,n,num_peaks,num_images, flip ;
     float  x,y,z,temp;
     float  tmp, alpha,r1,r2;
     char   *imagename, *profilename, *imagenumber, *temp_imagename,   *mergename,  *filename1, *filename2, *filename3, *maskfilename;

     
     imagename=(char *)calloc(200,sizeof(char));
     maskfilename=(char *)calloc(200, sizeof(char));
     profilename=(char *)calloc(200,sizeof(char));
     filename1=(char *)calloc(200,sizeof(char));
     filename2=(char *)calloc(200,sizeof(char));
     filename3=(char *)calloc(200,sizeof(char));
     imagenumber=(char *)calloc(200,sizeof(char));
     temp_imagename=(char *)calloc(200,sizeof(char));
     mergename=(char *)calloc(200,sizeof(char));
     

  
	FILE  *input, *binarymask, *output[3], *profile;
        
	strcpy(filename1,dirname);
     strcpy(filename2, filename1);
     strcpy(filename3,filename1);
     strcpy(maskfilename, dirname);

	strcat(maskfilename, "mask.pgm");
     strcat(filename1,"stack_whit.binary"); 
     strcat(filename2,"stack_ctf.binary"); 
     strcat(filename3,"average.pgm");
                     
     printf("\n\n::Input parameters local to each stack \n");  
     fflush(stdout);   
/* 
     binarymask=fopen(maskfilename,"r"); 
     if(binarymask==NULL)
     {
        printf("::ERROR  mask.pgm does not exist.");
        printf("::ML will continue but without masking the images \n");
        fflush(stdout);
        fclose(binarymask);
        return -1;
     }  
*/
     
              
     /*  input local parameters from 2dx_image.cfg  for each crystal image*/
          
	printf("\n::Directory  %s\n",dirname);
 

	strcpy(temp_imagename,dirname);
	strcat(temp_imagename,"/2dx_image.cfg");
	printf("configure file %s  \n\n",temp_imagename);
	fflush(stdout);
	
     input=fopen(temp_imagename,"r"); 
     if(input==NULL)
     {
        printf("::ERROR   2dx_image.cfg  does not exist.");
        printf("::ML will continue but without the stack from this directory \n");
        fflush(stdout);
        return -1;
     }
   	
     imagename=cgetline(input,"imagename");
     strcpy(mergename,imagename);
     strcat(imagename,".mrc");
     printf(":Image Name..................... = %s \n",imagename);
     strcpy(temp_imagename,dirname);
	strcat(temp_imagename, imagename);
     strcpy(imagename,temp_imagename);
     fflush(stdout);
       
   
	imagenumber=cgetline(input,"imagenumber");
     printf("::Image Number................... = %s \n",imagenumber);
     fflush(stdout);
     free(imagenumber);
       
     strcat(mergename,"-profile.dat");
     printf("Profile Name................... = %s \n",mergename);
     fflush(stdout);
       
     strcpy(temp_imagename,dirname);
     strcat(temp_imagename,mergename);
     strcat(profilename,temp_imagename);
     free(mergename);
     free(temp_imagename);
        
     if(fgetline(input, "CS",&CS)==0) 
        printf("CS............................. = %f  \n",CS);     
  
	if(fgetline(input, "KV", &KV )==0)
        printf("KV............................. = %f  \n",KV); 
  
     if(fgetline(input, "RESMIN", &RESMIN )==0)  
        printf("RESMIN......................... = %f  \n",RESMIN); 
 
	if(fgetline(input, "RESMAX",&RESMAX )==0)  
        printf("RESMAX......................... = %f  \n",RESMAX); 
  
	if(fgetline(input, "defocus", defocus)==0)
     { 
        DIFMID1=defocus[0];
        DIFMID2=defocus[1];
        ANGAST=defocus[2];
        printf("DFMID1,DFMID2,ANGAST........... = %f,%f,%f\n",DIFMID1,DIFMID2, ANGAST); 
     } 
      
     if(fgetline(input, "TLTAXIS", &TLTAXIS )==0)  
        printf("TLTAXIS......................... = %f  \n",TLTAXIS); 

     if(fgetline(input, "TLTANG", &TLTANG )==0)  
        printf("TLTANG......................... = %f  \n",TLTANG); 

     if(fgetline(input, "TLTAXA", &TLTAXA )==0)  
        printf("TLTAXA......................... = %f  \n",TLTAXA); 


     if(fgetline(input, "TAXA", &TAXA )==0)  
        printf("TAXA............................ = %f  \n",TAXA); 
              
              
     if(fgetline(input, "TANGL", &TANGL )==0)  
        printf("TANGL........................... = %f  \n",TANGL); 
	
	
	
	if(fgetline(input,"stepdigitizer",&DSTEP )==0)  
	{
        printf("DSTEP.......................... = %f\n",DSTEP); 
                 
        realcell_x1=(int)(realcell_x1_common*7/DSTEP);
        realcell_y1=(int)(realcell_y1_common*7/DSTEP);
                  
        if(fmod(realcell_x1,2.0)>0.5)
          realcell_x1++;
        if(fmod(realcell_y1,2.0)>0.5)
          realcell_y1++;
                  
     }
        
        
	if(fgetline(input,"magnification", &XMAG)==0)
        printf("Magnification (XMAG)........... = %f \n",XMAG);
 
//	if(Numstack==0)
     {
        if(fgetline(input,"phaori",phaori)==0)
	     printf("Phase Origin................... = %f %f\n",phaori[0],phaori[1]);
 
 	  if(fgetline(input,"lattice",lattice)==0)
          printf("Reciprocal Lattice............. = %f %f %f %f\n",lattice[0],lattice[1],lattice[2],lattice[3]);
	 }
     fclose(input);
        
         
         

    	/*    input images  */
 
	mrcImage *image = new mrcImage(imagename);
	free(imagename);
  
	sx=image->width(); sy=image->height(); 
        printf("sx=%d sy=%d \n",sx,sy);
        fflush(stdout);         
             
	unbend_image1=(float *)calloc(sx*sy,sizeof(float));
	unbend_image2=(float *)calloc(sx*sy,sizeof(float));
	unbend_image3=(float *)calloc(sx*sy,sizeof(float));
        printf("mode is %d \n",image->mode()); 
        fflush(stdout); 
    
 
	for(i=0;i<sx;i++) 
	  for(j=0;j<sy;j++) 
         {     
	 	if(image->mode()==0) temp=(float)((unsigned char*)image->rawData())[j*image->width()+i];
             	else if(image->mode()==1)  temp=(float)((unsigned short*)image->rawData())[j*image->width()+i];  
	     	else if(image->mode()==2) temp=(float)((float*)image->rawData())[j*image->width()+i];
             	else {printf("Unsupported mode.\n Exiting..."); exit(1);}
         
             	unbend_image1[j+i*sy]=temp;
	     	unbend_image2[j+i*sy]=temp;	    
	}   
  
 
  
     /*  Output the image size and lattice of the first crystal for calculating unit cell     */
  //    if(Numstack==0)
      {
         FILE *lat;
               
         lat=fopen("lattice.dat","w");
         fprintf(lat, "%d %d \n",sx,sy);
         fprintf(lat,"%f  %f  %f  %f  %f  %f     \n", DSTEP,XMAG,CS,KV, RESMAX,RESMIN);
               
         fprintf(lat, "%f %f %f %f\n",lattice[0],lattice[1],lattice[2],lattice[3]);
         fclose(lat);
	 }
   

	 	ctf_para[0]=CS;
		ctf_para[1]=KV;
		ctf_para[2]=DIFMID1;
		ctf_para[3]=DIFMID2;
		ctf_para[4]=ANGAST;
		ctf_para[5]=DSTEP;
		ctf_para[6]=XMAG;





	    if(fabs(TLTANG)<35 && fabs(TLTANG)>25 )
			for(i=0; i<sx; i++)
        	    			 for(j=0; j<sy; j++)
                     		unbend_image1[j+i*sy]=-unbend_image1[j+i*sy];

 
/*
	  for(i=0;i<sx; i++)
		for(j=0;j<sx;j++)
		unbend_image3[j+i*sx]=unbend_image1[j+i*sx];
*/
 
           if(do_whiten==1)
	    		whitening(sx,sy,unbend_image1, ctf_para);
 
	 
	    if(correct_CTF==1)		
         		ctfapply(sx,sy,unbend_image2, ctf_para);
  

  

 /*    input the peaks  */     
 
	 profile=fopen(profilename,"r");
         if(profile==NULL)
         {
	    printf("::ERROR   %s does not exists  \n",profilename );
	    printf("::ML will continue but without the stack from this directory \n");
	    fflush(stdout);  
	    return -1;
	  }
	
	 all_peak_counter=0;
         num_peaks=0;
      
	 int nc,nr,ic,ir,mina,maxa,minb,maxb;
	 float a1,a2,b1,b2,denmax;

	 char *c, oneline[120];
	 for(i=0;i<5;i++)
	    c=fgets(oneline,120,profile);
      
	 int read=fscanf(profile,"%d %d %d %d %f %f %f %f %d %d %d %d",&nc,&nr,&ic,&ir,&a1,&a2,&b1,&b2,&mina,&maxa,&minb,&maxb);
	 read=fscanf(profile,"%f",&denmax);

	 int dimension = (maxa-mina+1)*(maxb-minb+1);

	 if(dimension<201)
	 {    	printf("::ERROR: Not enough peaks in profile !!! (dimension=%d)\n",dimension);
           	printf("::MINA=%d, MAXA=%d, MINB=%d, MAXB=%d\n",mina,maxa,minb,maxb);
           	printf("::ABORTING.\n");
           	return -1;
      	}

	peak_x=(float *)calloc(dimension,sizeof(float));
      	peak_y=(float *)calloc(dimension,sizeof(float));
      	peak_z=(float *)calloc(dimension,sizeof(float));

      /*     Calculate Unit-cell center in pixels, using phaseorigin and the reciprocal space lattice   */
    //   if(Numstack==0)
      {
         float *real_lat;
         real_lat=(float *)calloc(4,sizeof(float));
                        
         reciprocal(lattice, real_lat, sx);

         x_center = real_lat[0] * phaori[0]/360.0 + real_lat[2] * phaori[1]/360.0;
         y_center = real_lat[1] * phaori[0]/360.0 + real_lat[3] * phaori[1]/360.0;
         
         printf(":\n");
         printf("&&&&&&&&&&&&&&  Phase Origin... = %f,%f\n",phaori[0],phaori[1]);
         printf(":Calculated x/y center offset... = %f,%f\n",x_center,y_center);
         printf(":\n");
         fflush(stdout);
         free(real_lat);
      }
 

 
	 /* Read peaks into core */
	for(i=0;i<dimension;i++)
      	{
         	read=fscanf(profile,"%f %f %f",&x,&y,&z);
         	peak_x[i]=x;   
         	peak_y[i]=y;  
                    
         	//      if(Numstack==0)
         	{   	peak_x[i]-=x_center;
             		peak_y[i]-=y_center;
         	}  
  
         	peak_z[i]=z;  
      	}
	 fclose(profile);

          
       /* sort peaks, here a simple bubble sort. Quicksort would be better (ToDo) */

	




	for(i=0;i<dimension-1;i++)
           for(j=dimension-1;j>i;j--)                      
	    if(peak_z[j]>peak_z[j-1])
           {
             	tmp=peak_x[j];
             	peak_x[j]=peak_x[j-1];
             	peak_x[j-1]=tmp;
             	tmp=peak_y[j];
             	peak_y[j]=peak_y[j-1];
             	peak_y[j-1]=tmp;
             	tmp=peak_z[j];
             	peak_z[j]=peak_z[j-1];
             	peak_z[j-1]=tmp;
          }

	  /* count peaks > 0.0 */
	for(i=dimension-1;i>=0;i--)
         if(peak_z[i]>0.0) break;
  
	all_peak_counter=i;

	if(all_peak_counter<2)
	{   	printf("::ERROR: Not enough non-zero peaks in profile !!! (all_peak_counter=%d)\n",all_peak_counter);
        	printf("::ABORTING.\n");
        	return -1;
       	}
 
	printf(":Total number of peaks ......... = %d\n",all_peak_counter);

	if(threshold_method==0)
	{	/* determine number of peaks above threshold */
	      	for(i=0;i<all_peak_counter;i++)
           	if(peak_z[i]<threshold) break;
             		num_peaks=i-1;
       }
       else
	{   	/* determined threshold value for inclusion of relative percentage */
        	num_peaks=(int)(all_peak_counter*relative_threshold/100.0);
           	threshold=peak_z[num_peaks];
           	printf("\n\n********* relative threshold is   %f \n", relative_threshold);
       	}

/*
	for(i=num_peaks; i>0; i--)
	     if(peak_z[i]>peak_z[10]*0.4)
		break;
  
	num_peaks=i;
*/
       
	printf(":Threshold ..................... = %f\n",threshold);
	printf(":Number of peaks above threshold = %d\n",num_peaks);
       	printf(": \n");
  
      /*  Output stacks of windowed particles into binary file     */  
    
	output[0]=fopen(filename1,"w");
       	output[1]=fopen(filename2,"w"); 
       	output[2]=fopen(filename3,"w");
       	free(filename1);
       	free(filename2);
      
       
       /*     Get  the  unit cell windows of whitened data */ 
	  Image1=(float *)calloc(500*realcell_x*realcell_y, sizeof(float));
	  Image2=(float *)calloc(500*realcell_x*realcell_y, sizeof(float));
	  Image3=(float *)calloc(500*realcell_x*realcell_y, sizeof(float));

	  float *temp_image, *pow_image, *SANG, *SFimage, *SFimage1;
	  int s;
          SANG=(float *)calloc(4,sizeof(float));
          SFimage=(float *)calloc(realcell_x*realcell_y,sizeof(float));
          SFimage1=(float *)calloc(realcell_x*realcell_y,sizeof(float)); 
	  temp_image=(float *)calloc(realcell_x*realcell_y*2,sizeof(float));
	  pow_image=(float  *)calloc(realcell_x/2,sizeof(float));
	


	  SANG[0]=TAXA;  
          SANG[1]=TLTANG;
          SANG[2]=TLTAXIS;
	  SANG[3]=num_peaks;



 //       if(fabs(SANG[1])>25 && fabs(SANG[1]<35))
//        if(fabs(SANG[1]<3))
 

	// if(fabs(SANG[1])<5.0)  				 
	//	SANG[2]=5;
			
       
       fwrite(SANG, sizeof(float)*4, 1, output[0]);
    
	
      
       fftwf_complex *in,*out;
       fftwf_plan p2_fw; 
       in=(fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*realcell_x*realcell_y);
       out=(fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*realcell_x*realcell_y); 
       
       p2_fw=fftwf_plan_dft_2d(realcell_x,realcell_x,in,out,FFTW_FORWARD,FFTW_ESTIMATE);	


       float *ave=(float *)calloc(realcell_x*realcell_y,sizeof(float));
       for(i=0;i<realcell_x;i++)
         for(j=0;j<realcell_y;j++)
        	ave[j+i*realcell_x]=0;
   
/*     
	  int *ma;
       ma=(int *)calloc(realcell_x*realcell_y, sizeof(int));

	  for(i=0;i<realcell_x;i++)
		for(j=0;j<realcell_y; j++)
		{	fscanf(binarymask, "%d", &ma[j+i*realcell_x]);
			if(ma[j+i*realcell_x]==255) ma[j+i*realcell_x]=0;
			else ma[j+i*realcell_x]=1;
		}
*/
	
	  int tag=0;
       m=0;
	  while(tag<num_peaks)
	  {       
	 		if(realcell_x1 == realcell_x1_common && realcell_y1==realcell_y1_common)
			{	 
	     	  	 if(num_peaks-tag<500)
	     	  	 {	 get_units_3D(&num_images,num_peaks-tag,tag, peak_x, peak_y, sx, sy, realcell_x1, realcell_y1, realcell_x, 	
						realcell_y, unbend_image1, unbend_image2, unbend_image3, Image1,Image2, Image3);      
	     	      	tag=num_peaks;
	     	  	 }
          		 else
            		 {	 get_units_3D(&num_images,500,tag,peak_x,peak_y,sx,sy,realcell_x1,realcell_y1, 
						realcell_x,realcell_y,unbend_image1, unbend_image2,unbend_image3, Image1, Image2,Image3);      
         		 		tag+=500;
            		 }
			}
	 		else 
			{
	     	  	 if(num_peaks-tag<500)
	       		 {  	get_units_merge_3D(&num_images,num_peaks-							    
							  tag,tag,peak_x,peak_y,sx,sy,realcell_x1,realcell_y1,realcell_x1_common,
                                realcell_y1_common,realcell_x,realcell_y,unbend_image1,unbend_image2, unbend_image3, Image1,  	
						  Image2, Image3);      
	        	 		tag=num_peaks;
	       		 }
            		 else
            		 {	 get_units_merge_3D(&num_images,500,tag,peak_x,peak_y,sx,sy,realcell_x1,realcell_y1,realcell_x1_common, 
						  realcell_y1_common, realcell_x,realcell_y,unbend_image1, unbend_image2, unbend_image3, Image1, 		
						  Image2,Image3);      
            		    	tag+=500;
            		 }	
	 		}   
  
			if(tag<1 && num_images==0)
        		{	 printf("::ERROR: ML_threshold is too high. No peaks above threshold.\n");
          		 printf("<<@progress: 30>>\n");
          		 fflush(stdout);
             		 return -1;
         		 }
			 

   
			normalize_image(num_images, realcell_x,realcell_y,Image1);
			normalize_image(num_images, realcell_x,realcell_y,Image2);  
//			normalize_image(num_images, realcell_x,realcell_y,Image3);
		 
			for(k=0; k<num_images; k++)
        		{      
            		 	for(i=0; i<realcell_x; i++)
        	    		for(j=0; j<realcell_y; j++)
              			 {//	if(do_whiten==0)
               	 		 //	SFimage[j+i*realcell_x]=-Image1[j+i*realcell_y+k*realcell_x*realcell_y];
					//	else 
						SFimage[j+i*realcell_x]=Image1[j+i*realcell_y+k*realcell_x*realcell_y];
              			}

/*
				 if(fabs(SANG[1])<35 && fabs(SANG[1])>25 )
				 for(i=0; i<realcell_x; i++)
        	    			 for(j=0; j<realcell_y; j++)
                     		SFimage[j+i*realcell_x]=-SFimage[j+i*realcell_x];
 				 else
*/

			if(fabs(SANG[1])<5.0)  
				 	Symmetrize(realcell_x, realcell_y, SFimage);
					 
  
               		 for(i=0;i<realcell_x;i++)
             			 for(j=0;j<realcell_y;j++)
                 			 ave[j+i*realcell_x]+=SFimage[j+i*realcell_x]; 
  

             		 for(i=0; i<realcell_x; i++)
        	  	  for(j=0; j<realcell_y; j++)
               		  {  	 in[j+i*realcell_x][0]=SFimage[j+i*realcell_x]*powf(-1.0,i+j);  
                    		 in[j+i*realcell_x][1]=0;
                	 }
              		 
              		 fftwf_execute_dft(p2_fw, in, out);
              		

        	   		 for(i=0; i<realcell_x; i++)
        	      		 for(j=0; j<realcell_y; j++)
	     	 		 {  /*
						 if(fabs(SANG[1])<35 && fabs(SANG[1]>25))
						{	if(i==realcell_y/2)
								out[j+i*realcell_x][0]=0;
								out[j+i*realcell_x][1]=0;
						}
						*/	
						 r1=exp(-(powf(i-realcell_x/2.0,2.0)+powf(j-realcell_y/2.0,2.0))/(rmax2*rmax2*4));
						 temp_image[j+i*realcell_y]=out[j+i*realcell_x][0]; // *r1;
                    		 		 temp_image[j+i*realcell_y+realcell_x*realcell_y]=out[j+i*realcell_x][1]; // *r1;
                		 }   	           	         	           	        
             		 			
				fwrite(temp_image, sizeof(float)*realcell_x*realcell_y*2, 1, output[0]);
 

				fwrite(SFimage,sizeof(float)*realcell_x*realcell_y,1,output[0]);



		/*	//if(k==300)  
			 for(i=0;i<realcell_x;i++)
             	 for(j=0;j<realcell_y;j++)
                  ave[j+i*realcell_x]+=sqrt(powf( temp_image[j+i*realcell_y],2.0)+powf(temp_image[j+i*realcell_y+realcell_x*realcell_y],2.0)); 
		*/      
                        
               		for(s=0; s<realcell_x/2;  s++)
                 		 pow_image[s]=0;
            
              		 for(i=0;i<realcell_x;i++)
	           		 for(j=0;j<realcell_y;j++)
                		 {   s=(int)(sqrtf((i-realcell_x/2)*(i-realcell_x/2)*1.0+(j-realcell_y/2)*(j-realcell_y/2)*1.0));
                
                    		 if(s>=rmax1 && s<=rmax2)
                    		 {
	                  		 pow_image[s]+=((powf(out[j+i*realcell_x][0],2.0)+powf(out[j+i*realcell_x][1],2.0)));
                       			 pow_image[0]+=((powf(out[j+i*realcell_x][0],2.0)+powf(out[j+i*realcell_x][1],2.0)));
                    		 }
                 		 }             		
              		 fwrite(pow_image, sizeof(float)*realcell_x/2, 1, output[0]);
 
              		 for(i=0; i<realcell_x; i++)
        	  	 	for(j=0; j<realcell_y; j++)
                   			SFimage[j+i*realcell_x]=Image2[j+i*realcell_y+k*realcell_x*realcell_y];
              		 translate(realcell_x, realcell_y, realcell_x/2, realcell_y/2,SFimage,SFimage1);    
   
              		 for(i=0; i<realcell_x; i++)
        	  	 	 for(j=0; j<realcell_y; j++)
                		 {     in[j+i*realcell_x][0]=SFimage1[j+i*realcell_x]*powf(-1.0,i+j);  
                     		 	in[j+i*realcell_x][1]=0;
                		 }
 
              		 
              		fftwf_execute_dft(p2_fw, in, out);
      
        		
			 	 for(i=0; i<realcell_x; i++)
         	      		 for(j=0; j<realcell_y; j++)
	            		{	temp_image[j+i*realcell_y]=out[j+i*realcell_x][0];
                    			temp_image[j+i*realcell_y+realcell_x*realcell_y]=out[j+i*realcell_x][1];
                 		}  	           	        
        	     	fwrite(temp_image, sizeof(float)*realcell_x*realcell_y*2, 1, output[1]);

        	     	m++;
        	}  // end of for(k) loop
 	 
	 }  // end of while(tag) loop
	
	
	//  write an image for analysis of algorithm


 
	float tempMax=-1.0e20, tempMin=-tempMax;
 	for(i=0;i<realcell_x;i++)
	  for(j=0;j<realcell_y;j++)
	   if(tempMax<ave[j+i*realcell_x]) tempMax=ave[j+i*realcell_x];
 	   else if(tempMin>ave[j+i*realcell_x]) tempMin=ave[j+i*realcell_x];
		
	fprintf(output[2],"P2 %d %d \n", realcell_x, realcell_y);
	fprintf(output[2],"255 \n");			
	for(i=0;i<realcell_x;i++)
	{
		for(j=0;j<realcell_y;j++)
			fprintf(output[2],"%d ", (int)((ave[j+i*realcell_x]-tempMin)/(tempMax-tempMin)*255));
//fprintf(output[2],"%f ", ave[j+i*realcell_x]);
		fprintf(output[2],"\n");
	}


 

/*
	for(i=0;i<sx;i++)
	  for(j=0;j<sy;j++)
	   if(tempMax<unbend_image1[j+i*sy]) tempMax=unbend_image1[j+i*sy];
 	   else if(tempMin>unbend_image1[j+i*sy]) tempMin=unbend_image1[j+i*sy];
		
	fprintf(output[2],"P2 %d %d \n", sx, sy);
	fprintf(output[2],"255 \n");			
	for(i=0;i<sx;i++)
	{
		for(j=0;j<sy;j++)
			fprintf(output[2],"%d ", (int)((unbend_image1[j+i*sy]-tempMin)/(tempMax-tempMin)*255));
		fprintf(output[2],"\n");
	}
*/
	fclose(output[2]);		
	

     printf("%d  unit cells are actually used \n", m);
     fflush(stdout);


	fclose(output[0]);
     fclose(output[1]);
    
      
     free(peak_x);
	free(peak_y);
	free(peak_z);
     free(unbend_image1);
     free(unbend_image2);
     free(Image1);
     free(Image2);
     free(temp_image);
     free(SFimage);
     free(SFimage1);
     free(SANG); 
 
     fftwf_destroy_plan(p2_fw);    
     fftwf_free(out);
     fftwf_free(in); 
     return 0; 
      
}
