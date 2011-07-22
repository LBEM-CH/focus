/*  
 *
 *  Created by Xiangyan Zeng on 1/1/2007
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *

*   This program merges  multiple crystals  and generates a reference using maximum likelihood merging algorithm 

*   It considers rotation and translation and can be used for merging 2d crystals
*  The rotation of the particles from a single stack is assumed to have Gaussian distribution 
*  The translation offset is assumed to have Gaussian distribution or uniforma distribution 
*
*   The stacks may have different rotation, translation, and noise statistics. A common structure is assumed for all 
*   the crystals to be merged and is obtained by the alignment of individual ML references. 
*
*  Input
*      nx, ny:  image patch size
*      num_images: number of image patches
*      Iteration: maximum iterations of maximum likelihood (normally the stop criterion is that the convergence of parameter is small enough)
*      Image: Stack of whitened image patches 
*      Image_CTF: Stack of CTF corrected image patches   
*   
*  Output
*      refer:                reference obtained from the whole dataset
*      refer1:               reference obtained from even numbers
*      refer2:               reference obtained from odd numbers 
*      Image_refer:
*      Image_refer_CTF:
*/


#include <string>
#include <stdio.h>
#include <stdlib.h>
#include <mrcImage.h>

#ifndef pi
#define pi 3.141592654
#endif

#define Max_num 1000            /*  Maximum number of particles processed at one time */

using namespace std;
 
 

void maximum_likelihood(int Numstack, int nx, int ny, char *dirfilename, float *refer, 
                        float *refer1, float *refer2, FILE *results)

{
       FILE  *input[2], *output[2];
        
       int CTF=1;
        

       float Gau=2.0;

       int i,j,k,m,n,k1,k2,k3,k4,sta,Loop,num_refer,Num_angles, num_images_stack, num_temp, Num_images,  flag[2];
       double *dev_sigma, *dev_x, *dev_y, *sigma, *dev_theta, *dev_sigma_theta, dev_sigma_theta_change;
       double *new_dev_sigma, *new_dev_x, *new_dev_y, *new_dev_theta, *new_sigma, *new_sigma_theta, dev_sigma_change;
 
       float *new_refer,*temp_image, *temp_image1, *temp_image2, *weight,*f,*corr_image,*corr_gama,*Image_refer,*Image_refer_CTF;
       float *angle, *temp_refer,*temp_refer1,*temp_refer2, *final_refer, *final_refer1, *final_refer2, *Image, *Image_CTF;
       double angle1,max,min,gama,corr,mean, pow_refer,pow_RT, likelihood;
       double st1, st2, st3, st4, st5,st6,st7;
       float pow_image,*RTrefer,*RTimage, *indx, *indy;
   
       char *filename1, *filename2, *stack_point;
       float *cry_angle;
       
       dev_sigma=(double *)calloc(Numstack,sizeof(double));
       dev_x=(double *)calloc(Numstack,sizeof(double));
       dev_y=(double *)calloc(Numstack,sizeof(double));
       dev_theta=(double *)calloc(Numstack,sizeof(double));
       sigma=(double *)calloc(Numstack,sizeof(double));
       dev_sigma_theta=(double *)calloc(Numstack,sizeof(double));
       
       new_dev_sigma=(double *)calloc(Numstack,sizeof(double));
       new_dev_x=(double *)calloc(Numstack,sizeof(double));
       new_dev_y=(double *)calloc(Numstack,sizeof(double));
       new_dev_theta=(double *)calloc(Numstack,sizeof(double));
       new_sigma=(double *)calloc(Numstack,sizeof(double));
       new_sigma_theta=(double *)calloc(Numstack,sizeof(double));
       
       
       cry_angle=(float *)calloc(Numstack,sizeof(float));
       
       filename1=(char *)calloc(200,sizeof(char));
       filename2=(char *)calloc(200,sizeof(char));    
    
       Num_angles=(int)((max_angle-min_angle)/step_angle)+1;
      
       i=0;

       int it = sizeof(float)*nx*ny;
       Image=(float *)calloc(Max_num*nx*ny,sizeof(float));    
       Image_CTF=(float *)calloc(Max_num*nx*ny,sizeof(float));    

       
  
       indx=(float *)calloc(nx*ny,sizeof(float));
       indy=(float *)calloc(nx*ny,sizeof(float));
       final_refer=(float *)calloc(nx*ny*Num_angles*Numstack,sizeof(float));
       final_refer1=(float *)calloc(nx*ny*Num_angles*Numstack,sizeof(float));
       final_refer2=(float *)calloc(nx*ny*Num_angles*Numstack,sizeof(float));
 
       temp_image=(float *)calloc(nx*ny,sizeof(float));
       temp_image1=(float *)calloc(nx*ny,sizeof(float));
       temp_image2=(float *)calloc(nx*ny,sizeof(float));
       temp_refer=(float *)calloc(nx*ny,sizeof(float));
       temp_refer1=(float *)calloc(nx*ny,sizeof(float));
       temp_refer2=(float *)calloc(nx*ny,sizeof(float));
       RTimage=(float *)calloc(nx*ny,sizeof(float));

       RTrefer=(float *)calloc(nx*ny*Num_angles,sizeof(float));
       weight=(float *)calloc(nx*ny*Num_angles,sizeof(float));
       new_refer=(float *)calloc(nx*ny*Num_angles*Numstack,sizeof(float));
       
       Image_refer=(float *)calloc(nx*ny*Iteration,sizeof(float));
       Image_refer_CTF=(float *)calloc(nx*ny*Iteration,sizeof(float));  

       corr_image=(float *)calloc(nx*ny*Num_angles,sizeof(float));
       corr_gama=(float *)calloc(nx*ny*Num_angles,sizeof(float));

       f=(float *)calloc(nx*ny*Num_angles,sizeof(float));

       angle=(float *)calloc(Num_angles,sizeof(float));

       printf("Memory  allocation done.\n");   fflush(stdout);
     
       output[0]=fopen("./SCRATCH/output1_ML_whitening.pgm","w");
       output[1]=fopen("./SCRATCH/output2_ML_CTFcorrect.pgm","w");
  
       fprintf(output[0],"P2 %d %d \n", 5*(ny+2), Iteration/5*(nx+2));
       fprintf(output[0],"256\n");
	     
       fprintf(output[1],"P2 %d %d \n", 5*(ny+2), Iteration/5*(nx+2));
       fprintf(output[1],"256\n");     
	

       printf(": \n");
       printf("::  Loop   Stack     NewSig     Dev_X      Dev_Y      Dev_Sigma    Dev_Theta Dev_Sigma_Theta\n");
       printf("::-----------------------------------------------------------------------------------------------------\n");
     

                Loop=0; 
                mrcImage::mrcHeader *header2 = mrcImage::headerFromData(nx,ny,2,(char*)refer);
                char outputfile [50];
                sprintf(outputfile,"SCRATCH/ML_reference_%03d.mrc",Loop);  
                mrcImage(header2,(char*)refer,outputfile);
                printf("File %s written.\n",outputfile);
                fprintf(results,"# IMAGE: %s <Reference Map %03d>\n",outputfile,Loop);
                fflush(results);

             
	
	 
        for(i=0;i<Num_angles;i++)
	    angle[i]=min_angle+i*step_angle;  
	
        /*   Preallocate the array position used for caculating the translation probability   */

         for(i=0;i<nx/2;i++)
            for(j=0;j<ny;j++)
	      indx[j+i*ny]=i;
	    
         for(i=nx/2;i<nx;i++)
            for(j=0;j<ny;j++)
	      indx[j+i*ny]=-(nx-i);    
	
         for(i=0;i<nx;i++)
            for(j=0;j<ny/2;j++)
	      indy[j+i*ny]=j;
	    

         for(i=0;i<nx;i++)
            for(j=ny/2;j<ny;j++)
	      indy[j+i*ny]=-(ny-j);
	    
        /*   Initialize the images  */ 	
         for(k=0;k<Iteration;k++)	
           for(i=0;i<nx;i++)
	     for(j=0;j<ny;j++)
	       {  Image_refer[j+i*ny+k*nx*ny]=0;
	          Image_refer_CTF[j+i*ny+k*nx*ny]=0;
               }  


  
	 for(i=0;i<nx;i++)
	    for(j=0;j<ny;j++)
	       {  Image_refer[j+i*ny]=refer[j+i*ny];
	          Image_refer_CTF[j+i*ny]=refer[j+i*ny];
               }
	
	 
	       
	 num_refer=1;
	
       
        /*  Initialize the parameters  */
        
        float *shx, *shy,  *SFimage;
	int  *flip;
	flip=(int *)calloc(Numstack,sizeof(int));
	shx=(float *)calloc(Numstack,sizeof(float));
	shy=(float *)calloc(Numstack,sizeof(float));
	SFimage=(float *)calloc(nx*ny,sizeof(float));

       
       for(sta=0;sta<Numstack;sta++)
       {     
	     sigma[sta]=1;  
             dev_sigma[sta]=5;   
	   
             dev_x[sta]=0;  
             dev_y[sta]=0;  
       
	     dev_theta[sta]=0;
	     dev_sigma_theta[sta]=3;  
	     
	     cry_angle[sta]=0;
             shx[sta]=0;
             shy[sta]=0;
             flip[sta]=0;
	
	}
	 
	 pow_image=0.0;
	 for(k3=0;k3<nx;k3++)
	     for(k4=0;k4<ny;k4++)
	         pow_image+=powf(refer[k4+k3*ny],2.0);
	
       
        /*       ML iteration            */     

        dev_sigma_change=1.0; 
        dev_sigma_theta_change=1.0;
        Loop=1;
	while((Loop<Iteration && (dev_sigma_change>Terminate_ML*Numstack || dev_sigma_theta_change>Terminate_ML*Numstack )) || Loop<5)  
         { 
	     
	      pow_refer=0.0;
	      for(k3=0;k3<nx;k3++)
	          for(k4=0;k4<ny;k4++)
		    pow_refer+=powf(refer[k4+k3*ny],2.0);
		 
	       
	        /*  Initialize the parameters that will be summation over all the particles */
	       
	       for(sta=0;sta<Numstack;sta++)
	       {
                    new_sigma[sta]=0;
	            new_dev_sigma[sta]=0;
		 
                    new_dev_x[sta]=0;
	            new_dev_y[sta]=0; 
		
		    new_dev_theta[sta]=0.0;
		    new_sigma_theta[sta]=0.0;
		}
		
		Num_images=0;
		 for(k=0; k<Numstack;k++)
 		      for(k2=0;k2<Num_angles;k2++)	 
			   for(k3=0;k3<nx;k3++)
			     for(k4=0;k4<ny;k4++)
			       {   
			       		 new_refer[k4+k3*ny+k2*nx*ny+k*nx*ny*Num_angles]=0; 
			       		 final_refer[k4+k3*ny+k2*nx*ny+k*nx*ny*Num_angles]=0; 
			       		 final_refer1[k4+k3*ny+k2*nx*ny+k*nx*ny*Num_angles]=0; 
			       		 final_refer2[k4+k3*ny+k2*nx*ny+k*nx*ny*Num_angles]=0; 
			       }    		 
			       		 
			       		 
		 
	        /*          Update  ML  Parameters          */	  

		likelihood=0.0;

        
             /*  Get stacks of particles */
             
                dev_sigma_change=0.0;
                dev_sigma_theta_change=0.0;
	        for(sta=0;sta<Numstack;sta++)
                  {  
                       /*   Open the files for reading data */
                        for(i=0;i<200;i++)
                             filename1[i]='\0';
     
                        j=0; 
                        while(dirfilename[j+sta*200]!='\n')  
                   	      {      filename1[j]=dirfilename[j+sta*200];
              	                       j++;  
                             }
                        filename1[j]='/';
                        strcpy(filename2, filename1);
                        strcat(filename1,"stack_whit.binary"); 
                        input[0]=fopen(filename1,"r");
                        if(input[0]==NULL) 
                        {
                              printf("\n stack_whit.binary does not exis \n  run 2dx_ML_stack.exe first \n");
                              exit(0);
                        }  
      
      
                        strcat(filename2,"stack_ctf.binary"); 
                        input[1]=fopen(filename2,"r");
                        if(input[1]==NULL) 
                        {
                              printf("\n stack_ctf.binary does not exis \n  run 2dx_ML_stack.exe first \n");
                              exit(0);
                        }  

                        fread(&cry_angle[sta], sizeof(float), 1, input[0]);
   printf("\n cry_angle=%f \n",cry_angle[sta]);                    

                        /*    Calculate the statistical parameters of translation */
	 
			for(k2=0;k2<Num_angles;k2++)
	    		 {  	 
			        for(k3=0;k3<nx;k3++)
                		     for(k4=0;k4<ny;k4++)
     	                              if (powf((float)(indx[k4+k3*ny]-dev_x[sta]),Gau)+powf((float)(indy[k4+k3*ny]-dev_y[sta]),Gau)<20*20)
 	  		               {    st1=powf((float)(indx[k4+k3*ny]-dev_x[sta]),Gau)+powf((float)(indy[k4+k3*ny]-dev_y[sta]),Gau);
				  	     st2=Gau*powf(dev_sigma[sta],Gau);
				             st3=powf((float)(angle[k2]-dev_theta[sta]),Gau);
					     st4=Gau*powf(dev_sigma_theta[sta],Gau);
	
                		             if(Num_angles>1)
                		             { 
					         st5=powf(dev_sigma[sta],Gau)*dev_sigma_theta[sta];
					         st5=1.0/(powf(sqrtf(2*pi),3.0)*st5+1.0e-20);			  			 
				                 f[k4+k3*ny+k2*nx*ny]=st5*exp(-st1/(st2+1.0e-20)-st3/(st4+1.0e-20));
					     }
                		            else     
                		            {   st5=powf(dev_sigma[sta],Gau);
					         st5=1.0/(powf(sqrtf(2*pi),2.0)*st5+1.0e-20);			  			 
				                 f[k4+k3*ny+k2*nx*ny]=st5*exp(-st1/(st2+1.0e-20));
                		             }
			              }	
  		                   else    f[k4+k3*ny+k2*nx*ny]=0;   	   
			 }
                 
                       
                      /*   Make rotated copies of the reference and normalize them by the initial reference */                
			for(k2=0; k2<Num_angles;k2++)
         		 {    
                              // 	  translate(nx,ny,-(int)shx[sta],-(int)shy[sta], refer,SFimage);
                                  
                               	  rotate(nx,ny,angle[k2]-cry_angle[sta],refer,RTimage);
                               
                             //  	  flip_image(nx,ny,RTimage, flip[sta]);
                                 
		               	  pow_RT=0;
	                       	  for(i=0;i<nx;i++)
	                       	        for(j=0;j<ny;j++)
	                       	               pow_RT+=powf(RTimage[j+i*ny],2.0);
		    
		               	  for(k3=0;k3<nx;k3++)
	                       	       for(k4=0;k4<ny;k4++)
	                       	              RTimage[k4+k3*ny]=RTimage[k4+k3*ny]*sqrt(pow_refer/pow_RT);
	                       
			          for(k3=0;k3<nx;k3++)
		                        for(k4=0;k4<ny;k4++)
		                              RTrefer[k4+k3*ny+k2*nx*ny]=RTimage[k4+k3*ny];                                      
        	          }
  
				
                       
			
                        
      
           
			fread(temp_image1, sizeof(float)*nx*ny, 1, input[0]);
                        fread(temp_image2, sizeof(float)*nx*ny, 1, input[1]);
                        num_temp=Max_num;
                        num_images_stack=0;
                        
                        if(feof(input[0])!=0 && feof(input[1])!=0)
                        {   printf("there are not particles extracted from the files \n");
                             fflush(stdout);
                             break;
                         } 
			else while(num_temp==Max_num)
                         {
                                num_temp=0;
				while(feof(input[0])==0 && num_temp<Max_num)  
                         	{     
                                	 for(i=0;i<nx;i++)
                                	      for(j=0;j<ny;j++)
                                	      {      Image[j+i*ny+num_temp*nx*ny]=temp_image1[j+i*ny];
                                                      Image_CTF[j+i*ny+num_temp*nx*ny]=temp_image2[j+i*ny];
                                               }
                                	num_temp++;
                                	fread(temp_image1, sizeof(float)*nx*ny, 1, input[0]);
                                	fread(temp_image2, sizeof(float)*nx*ny, 1, input[1]);
                         	}                  
                        	 
				Num_images+=num_temp;
				num_images_stack+=num_temp;
				
				if(num_temp<Max_num)
                              	 {   fclose(input[0]);
                              	      fclose(input[1]);
                              	 }
                            
 

             
				for(k1=0;k1<num_temp;k1++)
                        	  {    
                              	         for(i=0;i<nx;i++)
                                              for(j=0;j<ny;j++)
                                                   temp_image[j+i*ny]=Image[j+i*ny+k1*nx*ny];
                       
                           		   /*   Calculate cross correlation of the particles and the reference, using Fourier transform */
		            		  cross_corr(nx,ny,Num_angles,temp_image, RTrefer,corr_image);
  	 
	                    		  max=-1.0e20;   
                            		  for(k2=0;k2<Num_angles;k2++)
		                  		for(i=0;i<nx;i++)http: 
                                    		  for(j=0;j<ny;j++)
	                                  		if(corr_image[j+i*ny+k2*nx*ny]>=max) 
			              			max=corr_image[j+i*ny+k2*nx*ny];
				 
	                   		   gama=0;		       
		              	           for(k2=0;k2<Num_angles;k2++)
		                                   for(k3=0;k3<nx;k3++)
		                                        for(k4=0;k4<ny;k4++)
			                                  {      weight[k4+k3*ny+k2*nx*ny]=exp((corr_image[k4+k3*ny+k2*nx*ny]-max)/(sigma[sta]*sigma[sta]))*f[k4+k3*ny+k2*nx*ny]; 
			      	       
	                            		                  gama+=weight[k4+k3*ny+k2*nx*ny];
		                                          }
			     
		              		    likelihood+=log(gama)+max/(sigma[sta]*sigma[sta]+1.0e-20)-(pow_image+pow_refer)/(2*sigma[sta]*sigma[sta]+1.0e-20)-nx*ny*log(sigma[sta]);  	     

 

		            		    for(k2=0;k2<Num_angles;k2++)
		                   		for(k3=0;k3<nx;k3++)
		                         		for(k4=0;k4<ny;k4++)
			                       			weight[k4+k3*ny+k2*nx*ny]/=(gama+1.0e-20); 
        	       
	                       		 /*    Update the translation parameters   */
		              		     corr=0;
                            		     for(k2=0;k2<Num_angles;k2++)		 
                                     		for(k3=0;k3<nx;k3++)    //  translation in x
                                           		for(k4=0;k4<ny;k4++)  // translation in y
		                             		{     			   			    
                                                  		new_dev_sigma[sta]+=((powf(indx[k4+k3*ny]-dev_x[sta],Gau)+powf(indy[k4+k3*ny]-dev_y[sta],Gau))*weight[k4+k3*ny+k2*nx*ny]);  
				                   	        new_dev_x[sta]+=((indx[k4+k3*ny])*weight[k4+k3*ny+k2*nx*ny]);
			                                        new_dev_y[sta]+=((indy[k4+k3*ny])*weight[k4+k3*ny+k2*nx*ny]);
				  
				                                new_dev_theta[sta]+=(angle[k2]*weight[k4+k3*ny+k2*nx*ny]);
				                                new_sigma_theta[sta]+=(powf(angle[k2]-dev_theta[sta],Gau)*weight[k4+k3*ny+k2*nx*ny]); 
				
				                                corr+=(corr_image[k4+k3*ny+k2*nx*ny]*weight[k4+k3*ny+k2*nx*ny]);
			                                 }
 	
	                             	 /*         Update the noise std     */
		                  	        new_sigma[sta]+=(pow_image+pow_refer-2*corr);

               
		                     	/*  Calculate the reference weighted by the probability, using Fourier transform */
		                 		cross_corr(nx,ny,Num_angles,temp_image,weight,corr_gama);
		   
		                	        for(k2=0;k2<Num_angles; k2++) 
		                                      for(i=0;i<nx;i++)
	                                                     for(j=0;j<ny;j++)
		                                                    new_refer[j+i*ny+k2*nx*ny+sta*nx*ny*Num_angles]+=corr_gama[j+i*ny+k2*nx*ny];
	                           
      
                                         /*  If there are CTF corrected images, bring back the corresponding  references  */                
	                                        if(CTF==1)
	                         		{    
		                       			 for(i=0;i<nx;i++)
        	                           			 for(j=0;j<ny;j++)
        	                        				temp_image[j+i*ny]=Image_CTF[j+i*ny+k1*nx*ny]; 
          
		                         		 cross_corr(nx,ny,Num_angles,temp_image,weight,corr_gama);
		        
			                	         for(k2=0;k2<Num_angles; k2++) 
			                    			 for(i=0;i<nx;i++)
		                                     			for(j=0;j<ny;j++)
			                                		 {           final_refer[j+i*ny+k2*nx*ny+sta*nx*ny*Num_angles]+=corr_gama[j+i*ny+k2*nx*ny];
		                                                     		      if(fmod(k1*1.0,2.0)>=0.5)
					                                                   final_refer1[j+i*ny+k2*nx*ny+sta*nx*ny*Num_angles]+=corr_gama[j+i*ny+k2*nx*ny];
					                                              else
					                                                   final_refer2[j+i*ny+k2*nx*ny+sta*nx*ny*Num_angles]+=corr_gama[j+i*ny+k2*nx*ny];
					                                 }
	                                            }
		        
				}
				    
			}   // end of iterations over images in one  stack  
    	          
    	           
		   dev_sigma_change+=fabs(dev_sigma[sta]-powf(new_dev_sigma[sta]/(Gau*num_images_stack), 1.0/Gau));
	           dev_sigma_theta_change+=fabs(dev_sigma_theta[sta]-powf(new_sigma_theta[sta]/(num_images_stack), 1.0/Gau));
    	          
    	          
    	          sigma[sta]=sqrt(new_sigma[sta]/(num_images_stack*nx*ny));	 
	          dev_sigma[sta]=powf(new_dev_sigma[sta]/(Gau*num_images_stack), 1.0/Gau);		 
		  dev_x[sta]=new_dev_x[sta]/num_images_stack;
	          dev_y[sta]=new_dev_y[sta]/num_images_stack;	
	          
		  dev_theta[sta]=new_dev_theta[sta]/num_images_stack;
		  dev_sigma_theta[sta]=powf(new_sigma_theta[sta]/(num_images_stack), 1.0/Gau);
		 
    	          
    	          if(sta==0)
    	           {  printf("::%5d  %5d   %10.6f %10.6f %10.6f %12.6f  ",Loop,  sta, sigma[sta],dev_x[sta],dev_y[sta],dev_sigma[sta]);
	               printf("%12.6f %12.6f\n",dev_theta[sta],dev_sigma_theta[sta]); 
	            }
	          else
	            { printf("::%12d   %10.6f %10.6f %10.6f %12.6f  ",  sta, sigma[sta],dev_x[sta]+shx[sta],dev_y[sta]+shy[sta],dev_sigma[sta]);
	               printf("%12.6f %12.6f\n",dev_theta[sta],dev_sigma_theta[sta]); 
    	            }   
               }   // end of iteration over the stacks
	 
         
     
         
	       
	        if(CTF==1)
	        {     
	               //rot_refer_merge(Num_angles,Numstack, Num_images, cry_angle, angle,shx, shy, nx,ny,final_refer,temp_image);
	               
	          //     align_refer(Num_angles,Numstack, Num_images, cry_angle, angle,shx, shy, flip, nx,ny,final_refer);
	               
	               rot_refer_merge(Num_angles,Numstack, Num_images, cry_angle, angle, nx,ny,final_refer,temp_image);
	              

printf("\n cry_angle=%f %f \n",cry_angle[0],cry_angle[1]);
fflush(stdout);

		       
		       if(Symmetry>1)
	                       Symmetrize(nx,ny,temp_image);
		                 
		       rot_refer_merge(Num_angles,Numstack, Num_images, cry_angle, angle, nx,ny,final_refer1,refer1);   
		       rot_refer_merge(Num_angles,Numstack, Num_images, cry_angle, angle, nx,ny,final_refer2,refer2);   
		      
		       for(i=0;i<nx;i++)
			  for(j=0;j<ny;j++)
			     {  temp_refer[j+i*ny]=temp_image[j+i*ny];  
			         temp_refer1[j+i*ny]=refer1[j+i*ny];  
          	                 temp_refer2[j+i*ny]=refer2[j+i*ny];
         	            }
         	            
         	        mask(mask_radius,mask_radius,nx,ny,temp_image); 
	               for(i=0;i<nx;i++)
		              for(j=0;j<ny;j++)
		                    Image_refer_CTF[j+i*ny+num_refer*nx*ny]=temp_image[j+i*ny];	    
		 }    
          	     
      
	      /*   Rotate the reference back   */  
			 
	         rot_refer_merge(Num_angles,Numstack, Num_images, cry_angle, angle, nx,ny,new_refer,refer);
	         mask(mask_radius,mask_radius,nx,ny,refer); 
	        
	        /*   Apply symmetry if applicable   */
	        
		if(Symmetry>1)
	             Symmetrize(nx,ny,refer);
   
	        /*   Low pass filtering the reference to reduce the noise */
	           
	       
		pow_refer=0;
		for(i=0;i<nx;i++)
	           for(j=0;j<ny;j++)
	                pow_refer+=powf(refer[j+i*ny],2.0);
		 
		if(lp_radius>0.0 && lp_method>0)
	   	    low_pass(nx,ny,refer,lp_radius,lp_method);  
	   	    
	    	mask(mask_radius,mask_radius,nx,ny,refer);     		    
		  
		pow_RT=0;   
		for(i=0;i<nx;i++)
	           for(j=0;j<ny;j++)
		       pow_RT+=powf(refer[j+i*ny],2.0); 
		         
		for(k3=0;k3<nx;k3++)
	           for(k4=0;k4<ny;k4++)
	               refer[k4+k3*ny]=refer[k4+k3*ny]*sqrt(pow_refer/pow_RT);
	  	

               
                max=-1.0e20;
                min=-max;
                for(i=0;i<nx;i++)
                   for(j=0;j<ny;j++)
                   { if(refer[j+i*ny]>=max) max=refer[j+i*ny];
	             else if(refer[j+i*ny]<=min) min=refer[j+i*ny];
                   }

                for(i=0;i<nx;i++)
                   for(j=0;j<ny;j++)
	              temp_image[j+i*ny]=contrast*(refer[j+i*ny]-min)*256/(max-min)-(contrast-1.0)*128.0;  

                mrcImage::mrcHeader *header2 = mrcImage::headerFromData(nx,ny,2,(char*)temp_image);
                char outputfile [50];
                sprintf(outputfile,"SCRATCH/ML_reference_%03d.mrc",Loop);
                mrcImage(header2,(char*)temp_image,outputfile); 
                printf("File %s written.\n",outputfile);
                fprintf(results,"# IMAGE: %s <Reference Map %03d>\n",outputfile,Loop);
            
		for(i=0;i<nx;i++)
		   for(j=0;j<ny;j++)
		     Image_refer[j+i*ny+Loop*nx*ny]=refer[j+i*ny]; 
		     
	        mask(mask_radius,mask_radius,nx,ny,refer); 

                // printf("<<@evaluate>>\n");
                fflush(results);
                fflush(stdout);
                
		     
	        Loop++;
	        num_refer++;
 
	

        }	//  end of iteration of Loops	 
	
        if(Loop>=Iteration)
          printf(":Iteration maximum of %d reached.\n",Iteration);
        if(dev_sigma_change<=Terminate_ML && dev_sigma_theta_change<=Terminate_ML )
          printf(":ML termination due to convergence (dev-change below threshold of %f).\n",Terminate_ML);
	
        printf(":: \n");


/*    bring back the CTF corrected reference */
 
        if(CTF==1)  
         for(m=1;m<num_refer;m++)
	   {   
	        max=-1.0e20;
                min=-max;
                for(i=0;i<nx;i++)
                   for(j=0;j<ny;j++)
                   { if(Image_refer_CTF[j+i*ny+m*nx*ny]>=max) max=Image_refer_CTF[j+i*ny+m*nx*ny];
	             else if(Image_refer_CTF[j+i*ny+m*nx*ny]<=min) min=Image_refer_CTF[j+i*ny+m*nx*ny];
                   }

	   	      
                 for(i=0;i<nx;i++)
                   for(j=0;j<ny;j++)
	              temp_image[j+i*ny]=contrast*(Image_refer_CTF[j+i*ny+m*nx*ny]-min)*256/(max-min)-(contrast-1.0)*128.0;  
 	    
                  mrcImage::mrcHeader *header3 = mrcImage::headerFromData(nx,ny,2,(char*)temp_image);
                  char outputfile [50];
                  sprintf(outputfile,"SCRATCH/ML_reference_CTF_%03d.mrc",m);
                  mrcImage(header3,(char*)temp_image,outputfile);
                  printf("File %s written.\n",outputfile);
                  fprintf(results,"# IMAGE: %s <CTF-cor. Reference Map %03d>\n",outputfile,m);
                  fflush(results); 
                  // printf("<<@evaluate>>\n");
                  fflush(stdout);
 
        }		  
		 
  
/*          Write the references into a file  "output1.pgm "        */   
 
            
         for(k=0;k<Iteration;k++)
	    {  max=-1e20;min=1e20; 
               for(i=0;i<nx;i++)
                 for(j=0;j<ny;j++)
	           if(Image_refer[j+i*ny+k*nx*ny]>=max) max=Image_refer[j+i*ny+k*nx*ny];
	           else if(Image_refer[j+i*ny+k*nx*ny]<=min) min=Image_refer[j+i*ny+k*nx*ny];
	
                for(i=0;i<nx;i++)
                  for(j=0;j<ny;j++)
	             Image_refer[j+i*ny+k*nx*ny]=(Image_refer[j+i*ny+k*nx*ny]-min)*256/(max-min);  
	     }     
	 
	 
	  for(m=0;m<Iteration/5;m++)
	     {  for(i=0;i<nx;i++)
	         {  for(k=0;k<5;k++)
	               {   for(j=0;j<ny;j++)
                              fprintf(output[0],"%d ",(int)(Image_refer[j+i*ny+(k+m*5)*nx*ny]));   
                            fprintf(output[0],"%d %d  ", 256, 256);
                       }
	             fprintf(output[0],"\n");
                  }
	    
	   
	         for(j=0;j<5*(ny+2);j++)
	            fprintf(output[0],"%d ",256);
	     
	         fprintf(output[0],"\n");
	  
	         for(j=0;j<5*(ny+2);j++)
	            fprintf(output[0],"%d ",256);
	     
	         fprintf(output[0],"\n");
	 
	      }  
	     
 	 fclose(output[0]);     
 
 
/*  write the CTF-corrected references into a file "output2.pgm" */

         if(CTF==1)
            {
                for(k=0;k<Iteration;k++)
	         {  
		     max=-1e20;min=1e20; 
                     for(i=0;i<nx;i++)
                       for(j=0;j<ny;j++)
	                  if(Image_refer_CTF[j+i*ny+k*nx*ny]>=max) max=Image_refer_CTF[j+i*ny+k*nx*ny];
	                     else if(Image_refer_CTF[j+i*ny+k*nx*ny]<=min) min=Image_refer_CTF[j+i*ny+k*nx*ny];
	
                     for(i=0;i<nx;i++)
                        for(j=0;j<ny;j++)
	                    Image_refer_CTF[j+i*ny+k*nx*ny]=(Image_refer_CTF[j+i*ny+k*nx*ny]-min)*256/(max-min);  

	          }     
	 
	 
	        for(m=0;m<Iteration/5;m++)
	         {   
	            for(i=0;i<nx;i++)
	              {  for(k=0;k<5;k++)
	                    {    for(j=0;j<ny;j++)
	                            fprintf(output[1],"%d ",(int)(Image_refer_CTF[j+i*ny+(k+m*5)*nx*ny]));   
                                 fprintf(output[1],"%d %d  ", 256, 256);
                            }
	                 fprintf(output[1],"\n");
                      }
	   
	            for(j=0;j<5*(ny+2);j++)
	                fprintf(output[1],"%d ",256);
	     
	            fprintf(output[1],"\n");
	  
	            for(j=0;j<5*(ny+2);j++)
	                 fprintf(output[1],"%d ",256);
	     
	            fprintf(output[1],"\n");
	          }  
           }


        fclose(output[1]);
        
        
 

    /*    Return the references and aligned particles  */



        for(i=0;i<nx;i++)
             for(j=0;j<ny;j++)
		refer[j+i*ny]=temp_refer[j+i*ny];  
		
        for(i=0;i<nx;i++)
             for(j=0;j<ny;j++)
		refer1[j+i*ny]=temp_refer1[j+i*ny];  
		
        for(i=0;i<nx;i++)
             for(j=0;j<ny;j++)
		refer2[j+i*ny]=temp_refer2[j+i*ny];  	
    
          


     free(Image); 
     free(Image_CTF);
     free(indx); free(indy);
     free(final_refer);    
     free(final_refer1);  
     free(final_refer2);
     free(temp_image); 
     free(temp_refer); 
     free(temp_refer1);  
     free(temp_refer2);
     free(RTimage);  
     free(RTrefer);  
     free(weight);
     free(Image_refer);
     free(Image_refer_CTF);
    free(corr_image);
     free(corr_gama);
     free(angle);
     free(SFimage);
     free(flip);
     free(shx);
     free(shy);
     free(cry_angle);
     free(dev_sigma);
     free(dev_sigma_theta);
     free(dev_theta);
     free(dev_x);
     free(dev_y);
     free(f);
     free(filename1);
     free(filename2);
     free(new_dev_sigma);
     free(new_dev_theta);
     free(new_dev_x);
     free(new_dev_y);
     free(new_refer);
     free(new_sigma);
     free(new_sigma_theta);
     free(sigma);
     free(temp_image2);
      
 

 
  }			     
	 
