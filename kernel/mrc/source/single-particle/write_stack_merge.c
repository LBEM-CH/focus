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


float    RESMIN, RESMAX,  TLTAXA, phaori[2],lattice[4],defocus[3];


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ctf.c"
#include "whitening.c"
#include "realspace_lattice.c"
#include "reciprocal.c"
#include "get_units.c"
#include "normalize_image.c"
#include "2dx_lowpass.c"
#include "align.c"
using namespace std;
 

int write_stack_merge(int Numstack, char *dirname)
{
     
     
	float  *unbend_image1,*unbend_image2,  *Image1, *Image2;
        float  *peak_x, *peak_y, *peak_z;
        int    all_peak_counter, sta;
        int    sx,sy, i,j, k,m,n,num_peaks,num_images, flip ;
        float  x,y,z,temp;
        float  tmp, alpha,r1,r2;
        char   *imagename, *profilename, *imagenumber, *temp_imagename,   *mergename,  *filename1, *filename2;
	   float ctf_para[7];
	   float  CS, KV,  DIFMID1, DIFMID2, ANGAST, DSTEP, XMAG;
     

        imagename=(char *)calloc(200,sizeof(char));
        profilename=(char *)calloc(200,sizeof(char));
        filename1=(char *)calloc(200,sizeof(char));
        filename2=(char *)calloc(200,sizeof(char));
        imagenumber=(char *)calloc(200,sizeof(char));
        temp_imagename=(char *)calloc(200,sizeof(char));
        mergename=(char *)calloc(200,sizeof(char));
     

  
	FILE  *input, *output[2], *profile;
        
	strcpy(filename1,dirname);
        strcpy(filename2, filename1);
        strcat(filename1,"stack_whit.binary"); 
        strcat(filename2,"stack_ctf.binary"); 
       
                     
        printf("\n\n::Input parameters local to each stack \n");  
        fflush(stdout);   
              
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
         
        if(fgetline(input, "CS",&CS)==0); 
              printf("CS............................. = %f  \n",CS);     
  
	if(fgetline(input, "KV", &KV )==0)
              printf("KV............................. = %f  \n",KV); 
  
        if(fgetline(input, "RESMIN", &RESMIN )==0)  
              printf("RESMIN......................... = %f  \n",RESMIN); 
 
	if(fgetline(input, "RESMAX",&RESMAX )==0); 
              printf("RESMAX......................... = %f  \n",RESMAX); 

        if(fgetline(input, "TLTAXA",&TLTAXA )==0); 
              printf("TLTAXA......................... = %f  \n",TLTAXA); 

  
	if(fgetline(input, "defocus", defocus)==0)
            { 
                       DIFMID1=defocus[0];
                       DIFMID2=defocus[1];
                        ANGAST=defocus[2];
                        printf("DFMID1,DFMID2,ANGAST........... = %f,%f,%f\n",DIFMID1,DIFMID2, ANGAST); 
            } 
      
	
	if(fgetline(input,"stepdigitizer",&DSTEP )==0)  
                  printf("DSTEP.......................... = %f\n",DSTEP); 
      
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
        
     ctf_para[0]=CS;
	ctf_para[1]=KV;
	ctf_para[2]=DIFMID1;
	ctf_para[3]=DIFMID2;
	ctf_para[4]=ANGAST;
	ctf_para[5]=DSTEP;
	ctf_para[6]=XMAG;
         

    	  /*    input images  */
 
	mrcImage *image = new mrcImage(imagename);
	free(imagename);
  
	sx=image->width(); sy=image->height(); 
        printf("sx=%d sy=%d \n",sx,sy);
        fflush(stdout);         
             
	unbend_image1=(float *)calloc(sx*sy,sizeof(float));
	unbend_image2=(float *)calloc(sx*sy,sizeof(float));
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
        if(Numstack==0)
         {
               FILE *lat;
               
               lat=fopen("lattice.dat","w");
               fprintf(lat, "%d %d \n",sx,sy);
               fprintf(lat,"%f  %f  %f  %f  %f  %f     \n", DSTEP,XMAG,CS,KV, RESMAX,RESMIN);
               
               fprintf(lat, "%f %f %f %f\n",lattice[0],lattice[1],lattice[2],lattice[3]);
               fclose(lat);
	  }
     
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
      
	fscanf(profile,"%d %d %d %d %f %f %f %f %d %d %d %d",&nc,&nr,&ic,&ir,&a1,&a2,&b1,&b2,&mina,&maxa,&minb,&maxb);
	fscanf(profile,"%f",&denmax);

	int dimension = (maxa-mina+1)*(maxb-minb+1);

	 if(dimension<201)
	    {    printf("::ERROR: Not enough peaks in profile !!! (dimension=%d)\n",dimension);
                  printf("::MINA=%d, MAXA=%d, MINB=%d, MAXB=%d\n",&mina,&maxa,&minb,&maxb);
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
                    fscanf(profile,"%f %f %f",&x,&y,&z);
                    peak_x[i]=x;   
                    peak_y[i]=y;  
                    
              //      if(Numstack==0)
                    {        peak_x[i]-=x_center;
                    	     peak_y[i]-=y_center;
                     }  
                    peak_z[i]=z;  
             }
		fclose(profile);

          
         /* sort peaks, here a simple bubble sort. Quicksort would be better (ToDo) */
	    for(i=0;i<dimension-2;i++)
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
	       {        printf("::ERROR: Not enough non-zero peaks in profile !!! (all_peak_counter=%d)\n",all_peak_counter);
          	         printf("::ABORTING.\n");
                	 return -1;
               }
 
	     printf(":Total number of peaks ......... = %d\n",all_peak_counter);

	if(threshold_method==0)
	  {     /* determine number of peaks above threshold */
	              for(i=0;i<all_peak_counter;i++)
              		     if(peak_z[i]<threshold) break;
              	       num_peaks=i-1;
          }
        else
	 {     /* determined threshold value for inclusion of relative percentage */
                       num_peaks=(int)(all_peak_counter*relative_threshold/100.0);
                       threshold=peak_z[num_peaks];
                       printf("\n\n********* relative threshold is   %f \n", relative_threshold);
          }

       
	printf(":Threshold ..................... = %f\n",threshold);
	printf(":Number of peaks above threshold = %d\n",num_peaks);
        printf(": \n");
 

 
 
      /*  Output stacks of windowed particles into binary file     */  
    
	output[0]=fopen(filename1,"w");
        output[1]=fopen(filename2,"w"); 
        free(filename1);
        free(filename2);
        float tltaxis;
        
        tltaxis=TLTAXA;
        fwrite(&tltaxis, sizeof(float)*1, 1, output[0]);
            		
       
        /*     Get  the  unit cell windows of whitened data */ 
	Image1=(float *)calloc(500*realcell_x*realcell_y, sizeof(float));
	Image2=(float *)calloc(500*realcell_x*realcell_y, sizeof(float));
	
	float *temp_image, *pow_image;
	
	temp_image=(float *)calloc(realcell_x*realcell_y*2,sizeof(float));
	pow_image=(float  *)calloc(realcell_x,sizeof(float));
	
	
	fftwf_complex *in,*out;
        fftwf_plan p; 
        in=(fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*realcell_x*realcell_y);
        out=(fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*realcell_x*realcell_y); 
	
 
	
	int tag=0;
        m=0;
	while(tag<num_peaks)
	{      
	        if(num_peaks-tag<500)
	        {	get_units(&num_images,num_peaks-tag,tag,peak_x,peak_y,sx,sy,realcell_x1,realcell_y1,realcell_x,realcell_y,unbend_image1,unbend_image2, Image1,  Image2);      
	        	tag=num_peaks;
	        }
                else
                {	get_units(&num_images,500,tag,peak_x,peak_y,sx,sy,realcell_x1,realcell_y1,realcell_x,realcell_y,unbend_image1, unbend_image2, Image1, Image2);      
                 	tag+=500;
                 }	
	 
  
		if(tag<1 && num_images==0)
        	{      printf("::ERROR: ML_threshold is too high. No peaks above threshold.\n");
          		printf("<<@progress: 30>>\n");
             		fflush(stdout);
            	        return -1;
                 }
 
   
		normalize_image(num_images, realcell_x,realcell_y,Image1);
		normalize_image(num_images, realcell_x,realcell_y,Image2);  
		
		    
		for(k=0; k<num_images; k++)
        	{               
        	     	for(i=0; i<realcell_x; i++)
        	     	        for(j=0; j<realcell_y; j++)
	     		      temp_image[j+i*realcell_y]=Image1[j+i*realcell_y+k*realcell_x*realcell_y];   	           	        
             		fwrite(temp_image, sizeof(float)*realcell_x*realcell_y, 1, output[0]);
             		
             		for(i=0; i<realcell_x; i++)
        	     	        for(j=0; j<realcell_y; j++)
		     		      temp_image[j+i*realcell_y]=Image2[j+i*realcell_y+k*realcell_x*realcell_y];   	           	        
        	     	fwrite(temp_image, sizeof(float)*realcell_x*realcell_y, 1, output[1]);
        	     	
        	     	m++;
        	}
 	 
	}

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
        
        fftwf_free(out);
        fftwf_free(in); 
        return 0;
      
      
      

}
