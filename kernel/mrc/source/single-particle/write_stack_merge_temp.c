float  CS, KV, RESMIN, RESMAX, DIFMID1, DIFMID2, ANGAST, DSTEP, XMAG,phaori[2],lattice[4],defocus[3];
int do_whiten,correct_CTF;


#include "ctf.c"
#include "whitening.c"
#include "reciprocal.c"
#include "get_units.c"
#include "normalize_image.c"

 

int write_stack_merge()
{
     
     
      float  *unbend_image1,*unbend_image2, *temp_image, *Image1, *Image2;
      float  *peak_x, *peak_y, *peak_z, *real_lat;
      int    all_peak_counter;
      int    sx,sy, i,j, k,m,n,num_peaks,num_images ;
      float  x,y,z,temp;
      float  tmp;
      char   *imagename, *profilename, *imagenumber, *temp_imagename, *dirname, *imagecfg;

       imagename=(char *)calloc(400,sizeof(char));
       profilename=(char *)calloc(400,sizeof(char));
       imagenumber=(char *)calloc(200,sizeof(char));
       temp_imagename=(char *)calloc(200,sizeof(char));
       dirname=(char *)calloc(200,sizeof(char));
       imagecfg=(char *)calloc(200,sizeof(char));
     

       FILE  *input, *output[2];
    

       input=fopen("2dx_image.cfg","r"); 
    
   
       real_lat=(float *)calloc(4,sizeof(float)); 

       temp_image=(float *)calloc(realcell_x*realcell_y,sizeof(float));


   /*  input local parameters from 2dx_image.cfg to generate stacks*/

        

       imagename=cgetline(input,"imagename");
       strcpy(temp_imagename, imagename);
     //  strcpy(dirname,temp_imagename);
       strcat(imagename,".mrc");
       printf("Image Name.................... = %s \n",imagename);


       imagenumber=cgetline(input,"imagenumber");
          printf("Image Number.................. = %s \n",imagenumber);

    //   strcpy(profilename,"SCRATCH/prof");
   //    strcat(profilename,temp_imagename);
   //    strcat(profilename,".dat");
       strcpy(profilename,temp_imagename);
       strcat(profilename,"-profile.dat");
          printf("Profile Name.................. = %s \n",profilename);
       
       if(fgetline(input, "CS",&CS)==0); 
          printf("CS............................ = %f  \n",CS); 

       if(fgetline(input, "KV", &KV )==0)
          printf("KV............................ = %f  \n",KV); 

       if(fgetline(input, "RESMIN", &RESMIN )==0)  
          printf("RESMIN........................ = %f  \n",RESMIN); 

       if(fgetline(input, "RESMAX",&RESMAX )==0); 
          printf("RESMAX........................ = %f  \n",RESMAX); 
        
       if(fgetline(input, "defocus", defocus)==0)
         { 
              DIFMID1=defocus[0];
              DIFMID2=defocus[1];
              ANGAST=defocus[2];
              printf("DFMID1,DFMID2,ANGAST.......... = %f,%f,%f\n",DIFMID1,DIFMID2, ANGAST); 
          }
       
       if(*(cgetline(input,"ML_do_whiten"))=='y')
         {   do_whiten =1;
             printf("do_whiten..................... = yes \n");
         }
       else
         {   do_whiten=0;
             printf("do_whiten..................... = no \n");
         }

       if(*(cgetline(input,"ML_correct_CTF"))=='y')
         {   correct_CTF =1;
             printf("correct_CTF................... = yes \n");
         }
       else
         {   correct_CTF=0;
             printf("correct_CTF................... = no \n");
         }

       if(fgetline(input,"stepdigitizer",&DSTEP )==0)  
          printf("DSTEP......................... = %f\n",DSTEP); 
       
       if(fgetline(input,"magnification", &XMAG)==0)
          printf("Magnification (XMAG).......... = %f \n",XMAG);
       
       if(fgetline(input,"phaori",phaori)==0);
          printf("Phase Origin.................. = %f %f\n",phaori[0],phaori[1]);

       if(fgetline(input,"lattice",lattice)==0)
          printf("Reciprocal Lattice............ = %f %f %f %f\n",lattice[0],lattice[1],lattice[2],lattice[3]);

       fclose(input);

 

  

      /*    input images  */

      mrcImage *image = new mrcImage(imagename);
    
  
      sx=image->width(); sy=image->height(); 
      printf("sx=%d sy=%d \n",sx,sy);

       
      
      unbend_image1=(float *)calloc(sx*sy,sizeof(float));
      unbend_image2=(float *)calloc(sx*sy,sizeof(float));
  
      printf("mode is %d \n",image->mode()); 
    
      srandom(12379);
    
     
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
  
    // if(do_whiten==1)
       whitening(sx,sy,unbend_image1);
    
       if(correct_CTF==1)
           ctfapply(sx,sy,unbend_image2);
  
  
/*     Calculate Unit-cell center in pixels, using phaseorigin and the reciprocal space lattice */

       reciprocal(lattice, real_lat, sx);

       x_center = real_lat[0] * phaori[0]/360.0 + real_lat[2] * phaori[1]/360.0;
       y_center = real_lat[1] * phaori[0]/360.0 + real_lat[3] * phaori[1]/360.0;

       printf(":\n");
       printf("Phase Origin = %f,%f\n",phaori[0],phaori[1]);
       printf(":Calculated x/y center offset = %f,%f\n",x_center,y_center);
       printf(":\n");

 

 /*    input the peaks  */     
      
      input=fopen(profilename,"r");
      all_peak_counter=0;
      num_peaks=0;
      
      int nc,nr,ic,ir,mina,maxa,minb,maxb;
      float a1,a2,b1,b2,denmax;

      char *c, oneline[120];
      for(i=0;i<5;i++)
         c=fgets(oneline,120,input);
      
      fscanf(input,"%d %d %d %d %f %f %f %f %d %d %d %d",&nc,&nr,&ic,&ir,&a1,&a2,&b1,&b2,&mina,&maxa,&minb,&maxb);
      fscanf(input,"%f",&denmax);

      int dimension = (maxa-mina+1)*(maxb-minb+1);
 
      if(dimension<201)
      { printf("::ERROR: Not enough peaks in profile !!! (dimension=%d)\n",dimension);
        printf("::MINA=%d, MAXA=%d, MINB=%d, MAXB=%d\n",&mina,&maxa,&minb,&maxb);
        printf("::ABORTING.\n");
        return -1;
      }

      peak_x=(float *)malloc(sizeof(float)*dimension);
      peak_y=(float *)malloc(sizeof(float)*dimension);
      peak_z=(float *)malloc(sizeof(float)*dimension);

      /* Read peaks into core */
      for(i=0;i<dimension;i++)
      {
         fscanf(input,"%f %f %f",&x,&y,&z);
         peak_x[i]=x+x_center;  
         peak_y[i]=y+y_center;  
         peak_z[i]=z;  
      }
      fclose(input);

          
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
      {
        if(peak_z[i]>0.0) break;
      }

      all_peak_counter=i;

      if(all_peak_counter<2)
      { printf("::ERROR: Not enough non-zero peaks in profile !!! (all_peak_counter=%d)\n",all_peak_counter);
        printf("::ABORTING.\n");
        return -1;
      }

      // j=i+2;
      // if(j>=dimension) j=dimension;
      // for(i=0;i<j;i++)
      //   printf("peak[%6d] = %f %f %f\n",i,peak_x[i],peak_y[i],peak_z[i]);

      printf(":Total number of peaks ......... = %d\n",all_peak_counter);



      if(threshold_method==0)
      { /* determine number of peaks above threshold */
        for(i=0;i<all_peak_counter;i++)
          if(peak_z[i]<threshold) break;
        num_peaks=i-1;
      }
      else
      { /* determined threshold value for inclusion of relative percentage */
        num_peaks=(int)(all_peak_counter*relative_threshold/100.0);
        threshold=peak_z[num_peaks];
printf("\n\n************* relative threshold is %f \n", relative_threshold);
      }

      
      printf(":Threshold ..................... = %f\n",threshold);
      printf(":Number of peaks above threshold = %d\n",num_peaks);
      printf(": \n");
 
      printf("::Get particle stack \n"); 
      printf("<<@progress: 25>>\n");
      fflush(stdout);
 

/*     Get  the  unit cell windows  */ 

      Image1=(float *)malloc(sizeof(float)*num_peaks*realcell_x*realcell_y);
      Image2=(float *)malloc(sizeof(float)*num_peaks*realcell_x*realcell_y);
     
      get_units(&m,num_peaks,peak_x,peak_y,sx,sy,realcell_x1,realcell_y1,realcell_x,realcell_y,unbend_image1,Image1);
      get_units(&m,num_peaks,peak_x,peak_y,sx,sy,realcell_x1,realcell_y1,realcell_x,realcell_y,unbend_image2,Image2);
        
      num_images=m; 
   
      if(num_images==0)
      { printf("::ERROR: ML_threshold is too high. No peaks above threshold.\n");
        printf("<<@progress: 30>>\n");
        fflush(stdout);
        return -1;
      }
 
	 
/*   Normalize the particles  */ 	 

      normalize_image(num_images, realcell_x,realcell_y,Image1);
      normalize_image(num_images, realcell_x,realcell_y,Image2);  

    

/*  Output stacks of windowed particles into binary file */

      FILE *tt;
      float *refer;
      float mean, pow_image;

      refer=(float *)malloc(sizeof(float)*realcell_x*realcell_y);
      output[0]=fopen("stack_whit.binary","w");
      output[1]=fopen("stack_ctf.binary","w"); 


      for(i=0;i<realcell_x;i++)
        for(j=0;j<realcell_y;j++)
          refer[j+i*realcell_y]=0;
  

      for(i=0;i<num_images;i++)
        {   
            
            for(j=0;j<realcell_x;j++)
              for(k=0;k<realcell_y;k++)
	         temp_image[k+j*realcell_y]=Image1[k+j*realcell_y+i*realcell_x*realcell_y];
   
            fwrite(temp_image, sizeof(float)*realcell_x*realcell_y, 1, output[0]);

            for(j=0;j<realcell_x;j++)
              for(k=0;k<realcell_y;k++)
                temp_image[k+j*realcell_y]=Image2[k+j*realcell_y+i*realcell_x*realcell_y];

            fwrite(temp_image, sizeof(float)*realcell_x*realcell_y, 1, output[1]);


        }

printf("number of PATCHES in write-particles is %d \n",num_images); 

    
   



      free(unbend_image1);
      free(unbend_image2);
      free(Image1);
      free(Image2);

 
      fclose(output[0]);
      fclose(output[1]);  


      return 0;

}
