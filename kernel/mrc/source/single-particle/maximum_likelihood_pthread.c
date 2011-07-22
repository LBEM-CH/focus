/*  This program obtains the reference using maximum likelihood algorithm 

 *
 *  Created by Xiangyan Zeng on 1/1/2007
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *


It considers rotation and translation and can be used for single particle alignment and 2d crystals
The rotation is assumed to have Gaussian distribution 
The translation offset is assumed to have Gaussian distribution 


 A low-pass filter may be applied to the reference,


Input
   nx, ny:  image patch size
   num_images: number of image patches
   Iteration: maximum iterations of maximum likelihood (normally the stop criterion is that the convergence of parameter is small enough)
   Image: Stack of whitened image patches 
   Image_CTF: Stack of CTF corrected image patches   
   
Output
   refer:                reference obtained from the whole dataset
   refer1:               reference obtained from even numbers
   refer2:               reference obtained from odd numbers 
   Image_refer:
   Image_refer_CTF:
 

*/

//#include <sys/sysinfo.h>
#define Iteration 10 

 

#ifndef pi
#define pi 3.141592654
#endif

pthread_mutex_t lock1,lock2,lock3, lock4,lock5,lock6,lock7;

#include "MLpthread_n.c"


using namespace std;
 
 

void maximum_likelihood(int Num_images, int nx, int ny, float *Image, float *Image_CTF, float *refer, float *refer1, float *refer2, float *SANG)

{
       FILE  *output[3];
       int  Nthread;
       Nthread=7; // get_nprocs();
        
        
       pthread_attr_t* thAttr=NULL;
       my_struct  av[Nthread];
       pthread_t tid[Nthread];
       
 
       int Scale=1;
       int  rmax1=2, rmax2=(int)(0.35*nx/2);
       int shift=5;
       float Gau=2.0; 
       int CTF=1;
        
        fftwf_complex *in3,*out3;
        fftwf_plan  p3; 
       
       
        in3=(fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*nx*ny*nx);
        out3=(fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*nx*ny*nx); 
       
 //       int step_angle1,step_angle2, step_angle3,max_ang1, max_ang2,max_ang3;
        int ii,jj,i,j,k,m,m1,n,k1,k2,k3,k4,Loop,num_refer,Num_angles, ind, ind1,ind2,ind3,numstack;
       	int ang1,ang2,ang3;
        float dev_sigma, dev_x,dev_y,sigma[nx], dev_phi, dev_sigma_phi, dev_theta, dev_sigma_theta, dev_psi, dev_sigma_psi, phas, cosa, sina;
        float new_dev_sigma, new_dev_x, new_dev_y;
        float new_dev_phi, new_sigma_phi, new_dev_theta, new_sigma_theta, new_dev_psi, new_sigma_psi;
        float angle1,max,min,gama,mean, pow_RT, likelihood;
        float   dev_sigma_change,t1,st1,st2,st3,st5,phase;
 
        float *new_re_refer, *new_im_refer, *pdf, *new_sigma;
        float  *B,   *pow_refer;
        
  
        float  *par, *re, *im, *re_ref, *im_ref, *pow_image;
        int *num, *indx,*indy, error; 
	float *angles,*slice, *normal; 
        char *filename;
        
        filename=(char *)calloc(200,sizeof(char));
	 
	 
        Num_angles=((int)((2*max_ang1-1)/step_angle1)+1)*((int)((2*max_ang2-1)/step_angle2)+1)*((int)((2*max_ang3-1)/step_angle3)+1);    
          
	
	
	num=(int *)malloc(sizeof(int)*nx);
       
	par=(float *)calloc((nx/2+20)*Nthread,sizeof(float));
	new_sigma=(float *)calloc(nx/2,sizeof(float));
	 
	
	angles=(float *)calloc(3,sizeof(float));
	slice=(float *)calloc(nx*nx*nx*2,sizeof(float));
        normal=(float *)calloc(nx*nx*nx*Nthread,sizeof(float));
        new_re_refer=(float *)calloc(nx*nx*nx*Nthread, sizeof(float));
        new_im_refer=(float *)calloc(nx*nx*nx*Nthread, sizeof(float));
        
      
        pdf=(float *)calloc(nx*ny*Num_angles,sizeof(float));
        
    
        re=(float *)malloc(sizeof(float)*nx*ny*Num_images*Stack);  
        im=(float *)malloc(sizeof(float)*nx*ny*Num_images*Stack); 
        pow_image=(float *)malloc(sizeof(float)*nx*ny*Num_images*Stack); 

        re_ref=(float *)malloc(sizeof(float)*nx*ny*Num_angles);
        im_ref=(float *)malloc(sizeof(float)*nx*ny*Num_angles);
        pow_refer=(float *)malloc(sizeof(float)*nx*Num_angles);
         
      
        B=(float *)calloc(nx*nx*2,sizeof(float));
        indx=(int *)malloc(sizeof(int)*nx*nx);
        indy=(int *)malloc(sizeof(int)*nx*nx);
    
    

        printf("Allocation done.\n");
     
        output[0]=fopen("output1.pgm","w");
        output[1]=fopen("output2.pgm","w");
  
        fprintf(output[0],"P2 %d %d \n", 5*(ny+2), Iteration/5*(nx+2));
        fprintf(output[0],"256\n");
	     
        fprintf(output[1],"P2 %d %d \n", 5*(ny+2), Iteration/5*(nx+2));
        fprintf(output[1],"256\n");     
	 
	 /*
	float *defocus, mag,CHI_min, CHI_max, *CHImaxmin;
	defocus=(float *)calloc(3, sizeof(float));
	CHImaxmin=(float *)calloc(2,sizeof(float));
	
	mag=XMAG;
	defocus[0]=DIFMID1;
	defocus[1]=DIFMID2;
	defocus[2]= ANGAST;
	 */
	 
	 
      /*  Calculate    CTF  
       get_ctf(nx, ny, mag, defocus, ctf, CHI, CHImaxmin );
       CHI_max=CHImaxmin[0];
       CHI_min=CHImaxmin[1];
	*/
	
	                             
	
	/*   Calculate the power of images */           
      for(k1=0;k1<Num_images*Stack;k1++) 
       {  
   	 
	    pow_image[k1]=0;
            for(i=0;i<nx;i++)
	       for(j=0;j<ny;j++)
	         pow_image[k1]+=(powf(Image[j+i*ny+k1*nx*ny],2.0)+powf(Image[j+i*ny+k1*nx*ny+nx*ny*Num_images*Stack],2.0));
	 
            for(i=0;i<nx;i++)
	       for(j=0;j<ny;j++)
	        {    
	             re[j+i*ny+k1*nx*ny]=Image[j+i*nx+k1*ny*nx]; 
		     im[j+i*ny+k1*nx*ny]=Image[j+i*nx+k1*ny*nx+nx*ny*Num_images*Stack];    
	         /*
	             phas=-2*pi*(float)(i*nx/2+j*nx/2)/(float)nx; 
	             re[j+i*ny+k1*nx*ny]=Image[j+i*nx+k1*ny*nx]*cos(phas)-Image[j+i*nx+k1*ny*nx+nx*ny*Num_images*Stack]*sin(phas); 
		     im[j+i*ny+k1*nx*ny]=Image[j+i*nx+k1*ny*nx]*sin(phas)+Image[j+i*nx+k1*ny*nx+nx*ny*Num_images*Stack]*cos(phas);    
		  */   
                }
                
                  
            for(m=1; m<nx;  m++)
                   pow_image[k1+m*Num_images*Stack]=0;
            
            for(i=0;i<nx;i++)
	       for(j=0;j<ny;j++)
              {      m=(int)(sqrtf((i-nx/2)*(i-nx/2)*1.0+(j-ny/2)*(j-ny/2)*1.0));
                //    m=(int)(sqrtf(CHI[j+i*ny]-CHI_min)/sqrtf(CHI_max-CHI_min)*nx);
                
                 //   m=(int)((CHI[j+i*ny]-CHI_min)/(CHI_max-CHI_min)*nx+0.5);
                      if(m>=rmax1 && m<rmax2 )
	                pow_image[k1+m*Num_images*Stack]+=((powf(re[j+i*ny+k1*nx*ny],2.0)+powf(im[j+i*ny+k1*nx*ny],2.0)));
              }
           
        }
    
        
          /*   Preallocate the array position used for caculating the translation probability   */

            for(i=0;i<nx/2;i++)
                for(j=0;j<nx;j++)
	       	    indx[j+i*nx]=i;
	    
             for(i=nx/2;i<nx;i++)
                 for(j=0;j<nx;j++)
	             indx[j+i*nx]=-(nx-i);    
	
              for(i=0;i<nx;i++)
                 for(j=0;j<nx/2;j++)
	              indy[j+i*nx]=j;
	    
              for(i=0;i<nx;i++)
                 for(j=nx/2;j<nx;j++)
	      		indy[j+i*nx]=-(nx-j);
	                                
 
     
	 num_refer=1;
	 
 
      
        /*  Initialize the parameters  */
       
        dev_sigma=5;   
	   
        dev_x=0;  
        dev_y=0;  
        
        dev_phi=0;
	dev_sigma_phi=5;  
       
	dev_theta=0;
	dev_sigma_theta=5;  
	
	dev_psi=0;
	dev_sigma_psi=5;  
	
	
        float r1,r2;
        for(i=0;i<nx;i++) 
	      {   
		     r1=(float)random()/RAND_MAX;
		     r2=(float)random()/RAND_MAX;
		      
		     sigma[i]=1;  // powf(-2*log(r1),0.5)*cos(2*3.1415926*r2);     
               }

       
        for(i=0;i<nx;i++)
                    num[i]=0;

        for(i=0;i<nx;i++)
           for(j=0;j<ny;j++)
           {   
                	  m=(int)(sqrtf((i-nx/2)*(i-nx/2)*1.0+(j-ny/2)*(j-ny/2)*1.0));
             //   m=(int)(sqrtf(CHI[j+i*ny]-CHI_min)/sqrtf(CHI_max-CHI_min)*nx);
            //  m=(int)((CHI[j+i*ny]-CHI_min)/(CHI_max-CHI_min)*nx+0.5);
               		  if(m>=rmax1 && m<rmax2)    
                   		num[m]++;
            }
        
     
       
        /*       ML iteration            */     

        dev_sigma_change=1.0;
        Loop=1;

        ind1=nx*ny;
	ind2=(2*shift+1)*(2*shift+1);
	ind3=ind2*Num_angles;
	
          
          float powrefer=0.0;
	     

                powrefer=0;
                for(k3=0;k3<nx;k3++)
                   for(k4=0;k4<ny;k4++)
                       powrefer+=powf(refer[k4+k3*ny+nx/2*nx*ny],2.0);



             for(i=0;i<nx;i++)
               for(j=0;j<nx;j++)
                  for(k=0;k<nx;k++)
                       refer[k+i*nx+i*nx*nx]=refer[k+j*nx+i*nx*nx]*sqrt(1/powrefer);









		    
		//    printf("powrefer=%f \n", powrefer);
	                           
       
	while((Loop<Iteration && (dev_sigma_change>0.1 )) )  
         {     pthread_mutex_init(&lock1, NULL);
                pthread_mutex_init(&lock2, NULL);
                pthread_mutex_init(&lock3, NULL);
                pthread_mutex_init(&lock4, NULL);
                pthread_mutex_init(&lock5, NULL);
                pthread_mutex_init(&lock6, NULL);
                pthread_mutex_init(&lock7, NULL);
            
                for(i=0;i<200;i++)
			filename[i]='\0';
               strcpy(filename,"./results/cubic_");
               i=strlen(filename);
               filename[i]=Loop/10+'0';
               filename[i+1]=Loop-Loop/10*10+'0';
               strcat(filename,".dat");
               printf("Loop=%d   i=%d   %s  \n",Loop, i, filename);    
               
             
                output[2]=fopen(filename, "w");
                fwrite(refer, sizeof(float)*nx*nx*nx, 1, output[2]);
                fclose(output[2]);
                
                
                 output[1]=fopen("test2.pgm", "w");
                 max=-1.0e20; min=-max;
                 for(k=0;k<nx;k++) 
                    for(i=0;i<nx;i++)
                       for(j=0;j<nx;j++) 
                       {
                       		 if(max<refer[k+j*nx+i*nx*nx]) max=refer[k+j*nx+i*nx*nx];
                        	 if(min> refer[k+j*nx+i*nx*nx]) min=refer[k+j*nx+i*nx*nx];     
                       }
    
       
                 fprintf(output[1],"P2 %d %d \n 255 \n",nx,nx);   
                 k=nx/2;     
                 for(i=0;i<nx;i++)
                      for(j=0;j<nx;j++)
                        fprintf(output[1],"%d  ", (int)(( refer[k+j*nx+i*nx*nx]-min)/(max-min)*255));
                 fclose(output[1]);
                
                
              
                
                powrefer=0;
                for(k3=0;k3<nx;k3++)
	           for(k4=0;k4<ny;k4++)
		       powrefer+=powf(refer[k4+k3*ny+nx/2*nx*ny],2.0);  
		    
       
                printf("Loop=%d  powrefer=%e   refer=%f  \n",Loop,powrefer, refer[nx/2+nx/2*nx+nx/2*nx*nx]);
         
	        /*  FFT of 3D reference */
                for(i=0;i<nx;i++)
	            for(j=0;j<nx;j++)
	                for(k=0;k<nx;k++)
	                {   in3[k+j*nx+i*nx*nx][0]=refer[k+j*nx+i*nx*nx]*powf(-1.0,i+j+k);
                             in3[k+j*nx+i*nx*nx][1]=0;   
	                }
	        
                p3=fftwf_plan_dft_3d(nx,nx,nx, in3,out3,FFTW_FORWARD,FFTW_ESTIMATE);
                fftwf_execute(p3);
                fftwf_destroy_plan(p3);
              
       
         
                for(i=0;i<nx;i++)
	           for(j=0;j<ny;j++)
	               for(k=0;k<nx;k++)
	               {        slice[k+j*nx+i*nx*nx]=out3[k+j*nx+i*nx*nx][0]/sqrt(nx*nx*nx);
                                 slice[k+j*nx+i*nx*nx+nx*nx*nx]=out3[k+j*nx+i*nx*nx][1]/sqrt(nx*nx*nx);   
	               }
	               
	   
	                          
	               
	      
	      /*initialize the references ( central slices ) and the parameters   */
                for(n=0;n<Nthread;n++)
	            for(i=0;i<nx;i++)
	                for(j=0;j<nx;j++)
	                    for(k=0;k<nx;k++)
	                   {  
                                  new_re_refer[k+j*nx+i*nx*nx+n*nx*nx*nx]=0;
                                  new_im_refer[k+j*nx+i*nx*nx+n*nx*nx*nx]=0;
                                  normal[k+j*nx+i*nx*nx+n*nx*nx*nx]=0; 
	                    }
	                 
	            
                new_dev_sigma=0; 
                new_dev_x=0;
	        new_dev_y=0; 
		
		new_dev_phi=0;
	        new_sigma_phi=0;  
       
	        new_dev_theta=0;
	        new_sigma_theta=0;  
	
	        new_dev_psi=0;
	        new_sigma_psi=0;  
		 
                for(k3=0;k3<nx/2;k3++)
		        new_sigma[k3]=0.0; 
		
  
                for(n=0;n<Nthread;n++)
                    for(j=0;j<(nx/2+20);j++)
                        par[j+n*(nx/2+20)]=0;       
	                 
 
 
	        
	         /*    Calculate the statistical parameters of translation */

        	 
                k2=0;
                
               
                
                

                for(ang1=-max_ang1;ang1<max_ang1; ang1+=step_angle1)
              	      for(ang2=-max_ang2; ang2<max_ang2; ang2+=step_angle2)
              	           for(ang3=-max_ang3;ang3<max_ang3;ang3+=step_angle3) 
	     	           {   
	                                               
	          	      		for(k3=0;k3<nx;k3++)
		     	      		    for(k4=0;k4<ny;k4++)
		        	                 pdf[k4+k3*ny+k2*nx*ny]=0.0;
	                                             
	                                              
		  	                for(k3=0;k3<nx;k3++)
                   	                      for(k4=0;k4<ny;k4++)
 	              	                     {  	  i=indx[k4+k3*nx];
                                      j=indy[k4+k3*nx];
                                   
 	              	                             st1=powf((float)(indx[k4+k3*ny]-dev_x),Gau)+powf((float)(indy[k4+k3*ny]-dev_y),Gau);
				                     st2=Gau*powf(dev_sigma,Gau);
			                             st3=-st1/(st2+1.0e-20);

	 	//     if(abs(i)<=shift && abs(j)<=shift)  
	  //	printf("Loop=%d   dev_xy=%f %f   indxy=%d %d  st_xy=%f %f        ",Loop, dev_x,dev_y, indx[k4+k3*ny], indy[k4+k3*ny], st1,st2);  
			  
			                             st1=powf((float)(ang1-dev_phi),Gau);
			                             st2=Gau*powf(dev_sigma_phi,Gau);
			                             st3=st3-st1/(st2+1.0e-20);


 //   if(abs(i)<=shift && abs(j)<=shift)  		                             
//   printf(" st_angle=%f %f     \n ",st1,st2);			                             
			                 
			                             st1=powf((float)(ang2-dev_theta),Gau);
			                             st2=Gau*powf(dev_sigma_theta,Gau);
			                             st3=st3-st1/(st2+1.0e-20);
			  
			                             st1=powf((float)(ang3-dev_psi),Gau);
			                             st2=Gau*powf(dev_sigma_psi,Gau);
			                             st3=st3-st1/(st2+1.0e-20);
			                 
                         	  	             st5=powf(dev_sigma,Gau)*dev_sigma_phi*dev_sigma_theta*dev_sigma_psi;
		   			             st5=1.0/(powf(sqrtf(2*pi),5.0)*st5+1.0e-20);	
		   			 		 		  			 
	             			             pdf[k4+k3*ny+k2*nx*ny]=st5*exp(st3);     
	         			                            
		                               }	
 
          
                                         k2++; 		   
		               }
        
   printf("2222222222\n");      
      
                /*ML over all the stacks */
                numstack=0;         
	        for(numstack=0;numstack<Stack;numstack++)
                { 
	                //   Extract different projections of  the reference   
	           
                       k2=0; 
              	       for(ang1=-max_ang1;ang1<max_ang1; ang1+=step_angle1)
              	          for(ang2=-max_ang2; ang2<max_ang2; ang2+=step_angle2)
              	              for(ang3=-max_ang3;ang3<max_ang3;ang3+=step_angle3)
              	              {   
	                   		angles[0]=ang1+SANG[3*numstack];
	                   		angles[1]=ang2+SANG[3*numstack+1];
	                   		angles[2]=ang3+SANG[3*numstack+2];
	                     
	                      		for(i=0;i<nx;i++)
           					for(j=0;j<nx;j++)
           					{      B[j+i*nx]=0;
            			        		B[j+i*nx+nx*nx]=0;
          			 		}  
	                 
	                   		extract2D(B, angles, nx,nx,nx,slice);
                            
                         	        pow_RT=0;
		                        for(i=0;i<nx;i++)
	                                    for(j=0;j<ny;j++)
	                                    {
	                           
	                                         re_ref[j+i*nx+k2*nx*nx]=B[j+i*nx];
	                                         im_ref[j+i*nx+k2*nx*nx]=B[j+i*nx+nx*nx];
	                           
	                                        pow_RT+=powf(re_ref[j+i*nx+k2*nx*nx],2.0)+powf(im_ref[j+i*nx+k2*nx*nx],2.0);
        
                                            }
     
                                        for(i=0;i<nx;i++)
	                                     for(j=0;j<ny;j++)
	                                     {       
                                                     re_ref[j+i*nx+k2*nx*nx]=re_ref[j+i*nx+k2*nx*nx]*sqrt(powrefer/pow_RT);
	                                             im_ref[j+i*nx+k2*nx*nx]=im_ref[j+i*nx+k2*nx*nx]*sqrt(powrefer/pow_RT);
	                                      //         	 printf("ij=%d %d    %e %e \n    ", i,j, *(re_ref+j+i*nx+k2*nx*nx), *(im_ref+j+i*nx+k2*nx*nx));
                                             }
                             
                                       k2++;             
                             }    
 
 
                        //   calculate std of projections (references)  
              
                       k2=0;
                       for(ang1=-max_ang1;ang1<max_ang1; ang1+=step_angle1)
              	      	    for(ang2=-max_ang2; ang2<max_ang2; ang2+=step_angle2)
              	         	for(ang3=-max_ang3;ang3<max_ang3;ang3+=step_angle3) 
              	         	{  
              	               		for(m=0; m<nx/2;  m++)
                                       		pow_refer[k2+m*Num_angles]=0;
 
                               		for(i=0; i<nx; i++)
	                       			for(j=0; j<ny; j++)
                                      		{       m=(int)(sqrtf((i-nx/2)*(i-nx/2)*1.0+(j-ny/2)*(j-ny/2)*1.0));
                                        			  //  m=(int)(sqrtf(CHI[j+i*ny]-CHI_min)/sqrtf(CHI_max-CHI_min)*nx);
                                         			//      m=(int)((CHI[j+i*ny]-CHI_min)/(CHI_max-CHI_min)*nx+0.5);
                                       			 if(m>=rmax1 && m<rmax2 )
                                               		    pow_refer[k2+m*Num_angles]+=(powf(re_ref[j+i*ny+k2*nx*nx],2.0)+powf(im_ref[j+i*ny+k2*nx*nx],2.0));
                                      		 }                              
                               		 k2++;     
                                  }   


printf("Stack=%d   3333333333\n", numstack);	  
	 
	                  /*  Each stack is distributed to Nthread thread */
	                  
	               
                       for(n=0;n<Nthread;n++)
	               {	 
	           //            pthread_mutex_lock(&lock1);
	                 	av[n].rmax1=rmax1;
	                 	av[n].rmax2=rmax2;
	                 	av[n].shift=shift;
	                 	av[n].dim=nx; 
	                 	av[n].num_angles=Num_angles;  
	                 	av[n].num_images=Num_images;
	           	        av[n].start=numstack*Num_images+Num_images/Nthread*n;
	            		av[n].end=numstack*Num_images+Num_images/Nthread*(n+1);
	            		
	            		
	            	 	av[n].pdf=pdf;
	            		av[n].dev_x=dev_x;
	            		av[n].dev_y=dev_y;
	            		av[n].dev_phi=dev_phi;
	            		av[n].dev_theta=dev_theta;
	            		av[n].dev_psi=dev_psi;
	            		av[n].re_reference=re_ref;
	            		av[n].im_reference=im_ref;
	            		av[n].pow_reference=pow_refer;
	            		av[n].re_samp=re;
	            		av[n].im_samp=im;
	            		av[n].pow_samp=pow_image;
	            		av[n].sigma=sigma;
	            		
	            		av[n].angle=&SANG[3*numstack];
	            		av[n].new_re_refer=&new_re_refer[n*nx*nx*nx];
	            		av[n].new_im_refer=&new_im_refer[n*nx*nx*nx];
	            		av[n].normal=&normal[n*nx*nx*nx];
	            		av[n].new_par=&par[n*(nx/2+20)];	            
	              //         pthread_mutex_unlock(&lock1);
	          	
                        	error=pthread_create(&tid[n], thAttr, MLthread, (void*)&av[n]);
	     
	                }   // end of threading
	                
	               
	                   /*   wait for the ending of each thread  */
                        for(n=0;n<Nthread;n++)
                        {
                                     error=pthread_join(tid[n],NULL);
                                     printf(" thread %d termnates \n \n",error);
                        }                        
                
                                 
 /*                                 
    printf("n=%d      %f %f   %e \n",n, new_re_refer[nx/2+nx/2*nx+nx/2*nx*nx], new_im_refer[nx/2+nx/2*nx+nx/2*nx*nx],normal[nx/2+nx/2*nx+nx/2*nx*nx]);     
 */     
                
                
                }  // end of stack
    

       //         pthread_mutex_lock(&lock1);  
                 for(n=0;n<Nthread;n++)
	         {	  
                 			new_dev_sigma+=*(av[n].new_par);
                			new_dev_x+=*(av[n].new_par+1);
                 			new_dev_y+=*(av[n].new_par+2);
                 			new_dev_phi+=*(av[n].new_par+3);
                			new_dev_theta+=*(av[n].new_par+4);
                 			new_dev_psi+=*(av[n].new_par+5);
                 			new_sigma_phi+=*(av[n].new_par+6);
                 			new_sigma_theta+=*(av[n].new_par+7);
                 		 	new_sigma_psi+=*(av[n].new_par+8);
                 		//	for(i=0;i<nx/2;i++)
                 		//         if(num[i]>0)
                   				new_sigma[0]+=*(av[n].new_par+9);   
                   				   
                   		
              	   }    
        //         pthread_mutex_unlock(&lock1);
           
                 pthread_mutex_destroy(&lock1); 
                 pthread_mutex_destroy(&lock2); 
                 pthread_mutex_destroy(&lock3); 
                 pthread_mutex_destroy(&lock4); 
                 pthread_mutex_destroy(&lock5); 
                 pthread_mutex_destroy(&lock6); 
                 pthread_mutex_destroy(&lock7);                        
                              
     
     
     
     
    
    
             /*  transform the references back to the real space */
  
     //          printf("999999999999 \n");
              
                for(i=0;i<nx;i++)
                     for(j=0;j<nx;j++)
                           for(k=0;k<nx;k++) 
                           {    st1=0;
                                 st2=0;
                                 st3=0;
                                 for(n=0;n<Nthread;n++)
                                 {       st1+=new_re_refer[k+j*nx+i*nx*nx+n*nx*nx*nx];
                                          st2+=new_im_refer[k+j*nx+i*nx*nx+n*nx*nx*nx];
                                          st3+=normal[k+j*nx+i*nx*nx+n*nx*nx*nx];
                                  } 
                   	          if(st3>0)
                   	         { 
               			        in3[k+j*nx+i*nx*nx][0]=st1/st3*exp(-sqrt(powf(i-nx/2, 2.0)+powf(j-nx/2, 2.0)+powf(k-nx/2,2.0))/(nx/2));  
                		        in3[k+j*nx+i*nx*nx][1]=st2/st3*exp(-sqrt(powf(i-nx/2, 2.0)+powf(j-nx/2, 2.0)+powf(k-nx/2,2.0))/(nx/2));      
                   	          }
                   	         else
                  	         {
                        	        in3[k+j*nx+i*nx*nx][0]=0;
                		        in3[k+j*nx+i*nx*nx][1]=0;
                    	          }
              /*      	          
                   if(i==nx/2 && j==nx/2 && k==nx/2)
                   {
                         printf("new_refer=%e normal=%e  \n",st1,st3);
                         getchar();
                    }*/
                            }
   
                p3=fftwf_plan_dft_3d(nx,nx,nx, in3,out3,FFTW_BACKWARD,FFTW_ESTIMATE);
                fftwf_execute(p3);
                fftwf_destroy_plan(p3);
    
         
                for(i=0;i<nx;i++)
                    for(j=0;j<nx;j++) 
                       for(k=0;k<nx;k++)
                           refer[k+j*nx+i*nx*nx]=out3[k+j*nx+i*nx*nx][0]*powf(-1.0,i+j+k);
                     
	   
	        /*   Apply symmetry if applicable   */
	        
	//	if(Symmetry>1)
	//           Symmetrize(nx,ny,refer);
   
	        /*   Low pass filtering the reference to reduce the noise */
	          
	//	if(lp_radius>0.1)
	//   	    low_pass(nx,ny,refer,lp_radius,2);  		    
 		 

           /*      
		pow_refer=1;
		 
		pow_RT=0;   
		for(i=0;i<nx;i++)
	           for(j=0;j<ny;j++)printf("111111111 \n");
		       pow_RT+=powf(refer[j+i*ny],2.0); 
		         
		for(k3=0;k3<nx;k3++)
	           for(k4=0;k4<ny;k4++)
	               refer[k4+k3*ny]=refer[k4+k3*ny]*sqrt(pow_refer/pow_RT);
	  	 
                max=refer[0];
                min=max;
                for(i=0;i<nx;i++)
                   for(j=0;j<ny;j++)
                   { if(refer[j+i*ny]>=max) max=refer[j+i*ny];
	             else if(refer[j+i*ny]<=min) min=refer[j+i*ny];
                   }
              */
                
  	
               mask(nx-5,ny-5,nx,ny,refer);  
            
    
    
    
		 
		
		   
		dev_sigma_change=fabs(dev_sigma-powf(new_dev_sigma/(Gau*Num_images*Stack), 1.0/Gau));
		
	   
	//        dev_sigma_change=fabs(dev_sigma-powf(new_dev_sigma/(Gau*Num_images), 1.0/Gau));
	//        dev_sigma_theta_change=fabs(dev_sigma_theta-powf(new_sigma_theta/(Num_images), 1.0/Gau));
                
                k=0;  	   
                for(m=0; m<nx; m++)
                if(num[m]>0)
                    k+=num[m];
               
 
                sigma[0]=sqrtf(new_sigma[0]/(Stack*Num_images*k));	 
             




   printf("Loop=%d  sigma=%f  dev_x=%f dev_y=%f dev_sigma=%f ",Loop, sigma[0], dev_x,dev_y,dev_sigma);
	
	
	        printf("dev_phi=%e dev_sigma_phi=%e    dev_theta=%e dev_sigma_theta=%e   dev_psi=%e dev_sigma_psi=%e  \n",dev_phi, dev_sigma_phi, dev_theta, dev_sigma_theta, dev_psi, dev_sigma_psi  );



	        dev_sigma=powf(new_dev_sigma/(Gau*Num_images*Stack), 1.0/Gau);		 
		dev_x=new_dev_x/(Stack*Num_images);
	        dev_y=new_dev_y/(Stack*Num_images);	
	          
		dev_phi=new_dev_phi/(Stack*Num_images);
		dev_sigma_phi=powf(new_sigma_phi/(Num_images*Stack), 1.0/Gau);
		
		dev_theta=new_dev_theta/(Stack*Num_images);
		dev_sigma_theta=powf(new_sigma_theta/(Num_images*Stack), 1.0/Gau);
		
		dev_psi=new_dev_psi/(Stack*Num_images);
		dev_sigma_psi=powf(new_sigma_psi/(Num_images*Stack), 1.0/Gau);
		 
	
	
	printf("****** Loop=%d  sigma=%f  dev_x=%f dev_y=%f dev_sigma=%f ",Loop, sigma[0], dev_x,dev_y,dev_sigma);
	
	
	        printf("dev_phi=%e dev_sigma_phi=%e    dev_theta=%e dev_sigma_theta=%e   dev_psi=%e dev_sigma_psi=%e  \n",dev_phi, dev_sigma_phi, dev_theta, dev_sigma_theta, dev_psi, dev_sigma_psi  );	     
	//        getchar();

      //          for(m=0;  m<nx; m++)
     //           if(num[m]>0)
     //               printf("%e  ", sigma[m]);
     //           printf("\n");
 
		     
	        Loop++;
	        num_refer++;
 




 
        }	//  end of iteration of Loops	 
	
    


/*    bring back the CTF corrected reference


     
        if(CTF==1)  
         for(m=1;m<num_refer;m++)
	   {    
               
                 for(k2=0;k2<Num_angles;k2++)
	          for(i=0;i<nx;i++)
		    for(j=0;j<ny;j++)
		      new_refer[j+i*ny+k2*nx*ny]=final_refer[j+i*ny+k2*nx*ny+m*nx*ny*Num_angles]/Num_images;			

                 rot_refer(Num_angles, angle,nx,ny,new_refer,refer);


                  for(k2=0;k2<Num_angles;k2++)
	          for(i=0;i<nx;i++)
		    for(j=0;j<ny;j++)
		      new_refer[j+i*ny+k2*nx*ny]=final_refer1[j+i*ny+k2*nx*ny+m*nx*ny*Num_angles]/Num_images;			

                 rot_refer(Num_angles, angle,nx,ny,new_refer,refer1); 




                  for(k2=0;k2<Num_angles;k2++)
	          for(i=0;i<nx;i++)
		    for(j=0;j<ny;j++)
		      new_refer[j+i*ny+k2*nx*ny]=final_refer2[j+i*ny+k2*nx*ny+m*nx*ny*Num_angles]/Num_images;			

                 rot_refer(Num_angles, angle,nx,ny,new_refer,refer2);
      


	  
	   
	        if(Symmetry>1)
	        {  Symmetrize(nx,ny,refer);
                   Symmetrize(nx,ny,refer1);
                   Symmetrize(nx,ny,refer2);
                }
	    
		  
		 for(i=0;i<nx;i++)
		  for(j=0;j<ny;j++)
		     {  temp_refer[j+i*ny]=refer[j+i*ny];  
		        temp_refer1[j+i*ny]=refer1[j+i*ny];  
                        temp_refer2[j+i*ny]=refer2[j+i*ny];
                     }
		     
                


	 //	 mask(mask_radius,mask_radius,nx,ny,refer); 


		 
	          for(i=0;i<nx;i++)
		     for(j=0;j<ny;j++)
		         Image_refer_CTF[j+i*ny+m*nx*ny]=refer[j+i*ny];	
			 
        }		  
*/		


            fftwf_free(in3); 
            fftwf_free(out3);

 
 
 
  }			     
	 
