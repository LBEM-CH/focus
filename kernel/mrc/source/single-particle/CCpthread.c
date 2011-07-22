/* 		Align the particles 
		1.  using traditional cross-correlation calculated by fast Fourier transform 
		2.  or weighted cross_correlation

		implemented in multi-threaded programs.

*/ 

#include "simplex.c"

 
 void *CCthread(void* data)
 {          
            
    int Gau=2;
    int  rmax1, rmax2;
    int shift,Num_angles, Num_images;
 
    my_struct *pr;
    pr=(my_struct *)data;
             
    int ang1,ang2,ang3,ind,i,j,i1,j1,k,m,k1,k2,k3,k4, start,end,  ind1,ind2,ind3,nx,Loop;
    int *num, *indx,*indy, *count, *count1,*count2;
             
    float *pdf, *re_ref, *im_ref,  *pow_refer, *re, *im,*re_CTF, *im_CTF, *pow_image, *SANG, *sigma, *image;
    float *new_re_refer, *new_im_refer, *new_re_refer1, *new_im_refer1, *new_re_refer2, *new_im_refer2, *new_re_refer_CTF, *new_im_refer_CTF;
    float *normal, *normal1, *normal2, *normal_CTF, *new_par, *SFimage, *SFimage1;
    float dev_x,dev_y,dev_phi,dev_theta,dev_psi, wgt;
       
    
    float new_dev_sigma, new_dev_x, new_dev_y;
    float new_dev_phi, new_sigma_phi, new_dev_theta, new_sigma_theta, new_dev_psi, new_sigma_psi;
    float *new_sigma,  *B, dev_sigma_change,t1;
    float *corr, *weight, *angles, *corr_image, *corr_image_total,*amp1, *amp2;;

    int x_shift, y_shift;
    float ang1_rot, ang2_rot, ang3_rot;
         
    float gama=0.0, gama1,st1,st2, st3,st4, max,phas, max_max; 
 	double total_corr=0.0;
             
    /*  Get the data or address from the master thread*/   
    //      pthread_mutex_lock(&lock2); 

 
    Loop=(*pr).numLoop;
    rmax1=(*pr).rmax1;
    rmax2=(*pr).rmax2;
    shift=(*pr).shift; 
    nx=(*pr).dim;
    Num_angles=(*pr).num_angles;
    start=(*pr).start;
    end=(*pr).end;       
    pdf=(*pr).pdf;
    dev_x=(*pr).dev_x;
    dev_y=(*pr).dev_y;
    dev_phi=(*pr).dev_phi;
    dev_theta=(*pr).dev_theta;
    dev_psi=(*pr).dev_psi;
             
    re_ref=(*pr).re_reference; 
    im_ref=(*pr).im_reference;
 

    pow_refer=(*pr).pow_reference;
    re=(*pr).re_samp;
    im=(*pr).im_samp;
	image=(*pr).image;

    re_CTF=(*pr).re_samp_CTF;
    im_CTF=(*pr).im_samp_CTF;
    pow_image=(*pr).pow_samp;
    SANG=(*pr).angle;
    sigma=(*pr).sigma;
    new_re_refer=(*pr).new_re_refer;
    new_im_refer=(*pr).new_im_refer;

    new_re_refer1=(*pr).new_re_refer1; 
    new_im_refer1=(*pr).new_im_refer1;
    new_re_refer2=(*pr).new_re_refer2; 
    new_im_refer2=(*pr).new_im_refer2;

    normal=(*pr).normal;
count=(*pr).count;

    normal1=(*pr).normal1;
count1=(*pr).count1;

    normal2=(*pr).normal2;
count2=(*pr).count2;

    new_re_refer_CTF=(*pr).new_re_refer_CTF;
    new_im_refer_CTF=(*pr).new_im_refer_CTF;
    normal_CTF=(*pr).normal_CTF;
    new_par=(*pr).new_par;
   //         pthread_mutex_unlock(&lock2); 

              
    			      	     
    			      	     
 
    new_sigma=(float *)calloc(nx/2,sizeof(float));
            
    indx=(int *)malloc(sizeof(int)*nx*nx);
    indy=(int *)malloc(sizeof(int)*nx*nx);
    num=(int *)malloc(sizeof(int)*nx);
       
    weight=(float *)malloc(sizeof(float)*nx*nx*Num_angles); 
    angles=(float *)calloc(3,sizeof(float));
    B=(float *)calloc(nx*nx*2,sizeof(float));
        

	SFimage=(float *)calloc(nx*nx,sizeof(float));
	SFimage1=(float *)calloc(nx*nx,sizeof(float));

    amp1=(float *)calloc(nx/2, sizeof(float));
    amp2=(float *)calloc(nx/2, sizeof(float));      
    corr=(float *)calloc(nx/2, sizeof(float));	               
   
    corr_image=(float *)malloc(sizeof(float)*nx*Num_angles*(2*shift+1)*(2*shift+1));
    corr_image_total=(float *)malloc(sizeof(float)*Num_angles*(2*shift+1)*(2*shift+1));
           
            
    pthread_mutex_lock(&lock3); 
    fftwf_complex *in,*out;
    fftwf_plan p1, p2; 
    in=(fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*nx*nx);
    out=(fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*nx*nx); 
        
    p1=fftwf_plan_dft_2d(nx,nx,in,out, FFTW_FORWARD,FFTW_ESTIMATE);
    p2=fftwf_plan_dft_2d(nx,nx,out,in,FFTW_BACKWARD,FFTW_ESTIMATE);
    pthread_mutex_unlock(&lock3);     
                      
              
  
        
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
	    
        
        
    /*  Initialize the parameters that will be summation over all the particles */
	        
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

   
    for(i=0;i<nx;i++)
       num[i]=0;

    for(i=0;i<nx;i++)
       for(j=0;j<nx;j++)
       {   
        	  m=(int)(sqrtf((i-nx/2)*(i-nx/2)*1.0+(j-nx/2)*(j-nx/2)*1.0));
       	  if(m>=rmax1 && m<rmax2)    
        		num[m]++;
       }
        
               
		       
     /*          Update  ML  Parameters  by each particles        */	   
     ind1=nx*nx;
	ind2=(2*shift+1)*(2*shift+1);
	ind3=ind2*Num_angles;
	

        
	for(k1=start;k1<end;k1++)
     {     
                     
   		//	printf(" k1=%d  333333333 in thread \n", k1);
  		//  fflush(stdout);

        /*   Calculate cross correlation of the particles and the reference in Fourier space  */  

if(Loop>1)
{ 



/*		Calculate weighted cross correlation 

        k2=0;
        for(ang1=-max_ang1;ang1<=max_ang1;ang1+=step_angle1)
          for(ang2=-max_ang2;ang2<=max_ang2;ang2+=step_angle2)
            for(ang3=-max_ang3;ang3<=max_ang3;ang3+=step_angle3)
            {               		  			     	 
    		     for(k3=-shift; k3<=shift;  k3++)
                 for(k4=-shift; k4<=shift; k4++)
                 {   	ind=k4+shift+(k3+shift)*(2*shift+1)+k2*ind2; 
                      	corr_image_total[ind]=0.0;
  	
					for(m=0;m<nx/2;m++)
					{	amp1[m]=0.0;
						amp2[m]=0.0;
						corr[m]=0.0;
					}


				  	for(i=0;i<nx;i++)
        	  	  	    		for(j=nx/2;j<nx;j++)
          	 	    		{  	m=(int)(sqrtf((i-nx/2)*(i-nx/2)*1.0+(j-nx/2)*(j-nx/2)*1.0));
          	       	   			if(m>=rmax1 && m<=rmax2)
                    	  			{ 	 

								phas=-2*pi*(float)((i+nx/2)*k3+(j+nx/2)*k4)/(float)nx;

								i1=j+i*nx+k2*nx*nx;
 					     			st2=re_ref[i1]*cos(phas)-im_ref[i1]*sin(phas);
                      	     					st3=re_ref[i1]*sin(phas)+im_ref[i1]*cos(phas); 
								amp1[m]+=powf(st2,2.0)+powf(st3,2.0);

								j1=j+i*nx+k1*nx*nx;
								amp2[m]+=powf(re[j1],2.0)+powf(im[j1],2.0);

								corr[m]+=re[j1]*st2+im[j1]*st3;         	 
					  	}
				    	}  


				 
				 	for(m=rmax1; m<=rmax2; m++)
				 	{		
							wgt=exp(-m*m/powf(rmax2*0.7,2.0));

							st2=corr[m]/sqrt(amp1[m]*amp2[m]+1.0e-10);

						// 	corr_image_total[ind]+=fabs(powf(st2,3.0))*wgt;
							
						 	corr_image_total[ind]+=(powf(st2,1.0))*wgt;

				  	}
        	
	             }
	            		         	
	             k2++;            		     
  	       }	 
 
 */



/*   Calculate unweighted cross correlation NOT using fast Fourier transform

        k2=0;
        for(ang1=-max_ang1;ang1<=max_ang1;ang1+=step_angle1)
          for(ang2=-max_ang2;ang2<=max_ang2;ang2+=step_angle2)
            for(ang3=-max_ang3;ang3<=max_ang3;ang3+=step_angle3)
            {               		  			     	 
    		     for(k3=-shift; k3<=shift;  k3++)
                 for(k4=-shift; k4<=shift; k4++)
                 {    ind=k4+shift+(k3+shift)*(2*shift+1)+k2*ind2; 
                       corr_image_total[ind]=0.0;
 
				   for(i=nx/2;i<nx;i++)
        	  	  	    for(j=0;j<nx;j++)
          	 	    {   m=(int)(sqrtf((i-nx/2)*(i-nx/2)*1.0+(j-nx/2)*(j-nx/2)*1.0));
          	       	    if(m>=rmax1 && m<=rmax2)
                    	    { 	
							wgt=exp(-m*m/powf(rmax2*0.7,2.0));

							phas=-2*pi*(float)((i+nx/2)*k3+(j+nx/2)*k4)/(float)nx;

							i1=j+i*nx+k2*nx*nx;

							j1=j+i*nx+k1*nx*nx;


							st2=re_ref[i1]*cos(phas)-im_ref[i1]*sin(phas);
                      	     	st3=re_ref[i1]*sin(phas)+im_ref[i1]*cos(phas); 
							corr_image_total[ind]+=(re[j1]*st2+im[j1]*st3)*wgt;
 
 					    // 	st2=(*(re_ref+j+i*nx+k2*nx*nx))*cos(phas)-(*(im_ref+j+i*nx+k2*nx*nx))*sin(phas);
                      	    // 	st3=(*(re_ref+j+i*nx+k2*nx*nx))*sin(phas)+(*(im_ref+j+i*nx+k2*nx*nx))*cos(phas); 
                             //		corr_image_total[ind]+=(*(re+j+i*nx+k1*nx*nx))*st2+(*(im+j+i*nx+k1*nx*nx))*st3;

					    }
				    }          	
	             }
	            		         	
	           k2++;            		     
  	       }	 
 
*/ 






//			Calculate unweighted cross correlation using fast Fourier transform

 
        k2=0;
        for(ang1=-max_ang1;ang1<=max_ang1;ang1+=step_angle1)
          for(ang2=-max_ang2;ang2<=max_ang2;ang2+=step_angle2)
            for(ang3=-max_ang3;ang3<=max_ang3;ang3+=step_angle3)
	   {	
	     	// if(*(SANG+1)>5 || (*(SANG+1)<5 && fabs((float)ang1)<=max_ang1/2 && fabs((float)ang3)<=max_ang3/2 && fabs((float)ang2)<=1))

	//	if(  *(SANG+1)>5.0     ||     ( *(SANG+1)<5.0  &&  fabs((float)ang2)<=max_ang2/2   && fabs((float)ang1)<=max_ang1/2  &&  fabs((float)ang3)<=max_ang3/2 ) )   
            	{               		 
               		 for(i=0;i<nx;i++)
        	  		for(j=0;j<nx;j++)
          	  		{       m=(int)(sqrtf((i-nx/2)*(i-nx/2)*1.0+(j-nx/2)*(j-nx/2)*1.0));

				
          	       	  		if(m>=rmax1 && m<rmax2)
          	         		{ 
						wgt=exp(-m*m/powf(rmax2*0.6,2.0));
          				      	out[j+i*nx][0]=re[j+i*nx+k1*nx*nx]*re_ref[j+i*nx+k2*nx*nx]+im[j+i*nx+k1*nx*nx]*im_ref[j+i*nx+k2*nx*nx];
	      				      	out[j+i*nx][1]= -re[j+i*nx+k1*nx*nx]*im_ref[j+i*nx+k2*nx*nx]+im[j+i*nx+k1*nx*nx]*re_ref[j+i*nx+k2*nx*nx]; 

						out[j+i*nx][0]*=wgt;
						out[j+i*nx][1]*=wgt;
			
       					  } 
	      				  else
	      				  {	out[j+i*nx][0]=0.0;
	      					out[j+i*nx][1]=0.0;
	      				  }
	    		  	}	 
	    			   
		     	fftwf_execute(p2);
    			     	 
    		     	for(k3=0; k3<nx;  k3++)
                 		for(k4=0; k4<nx; k4++)
                 		{   
                    			i=indx[k4+k3*nx];
                    			j=indy[k4+k3*nx];
        
                    			if(abs(i)<=shift && abs(j)<=shift)
                    			{
                    				ind=j+shift+(i+shift)*(2*shift+1)+k2*ind2; 	
                         			corr_image_total[ind]=in[k4+k3*nx][0]*powf(-1.0,k3+k4)/(nx); 
	              			}	
	           		}
		}  // end of IF
	            		         	
	        k2++;            		     
  	  }	 

 

	 max=-1.0e20;  
   	 
       	 k2=0;
	  for(ang1=-max_ang1;ang1<=max_ang1;ang1+=step_angle1)
            for(ang2=-max_ang2;ang2<=max_ang2;ang2+=step_angle2)
            	for(ang3=-max_ang3;ang3<=max_ang3;ang3+=step_angle3)
{
    		// if(  *(SANG+1)>5.0     ||     ( *(SANG+1)<5.0    &&    fabs((float)ang1)<=max_ang1/2    &&    fabs((float)ang3)<=max_ang3/2   &&  fabs((float)ang2)<=1 )  )  

	//	if(  *(SANG+1)>5.0     ||     ( *(SANG+1)<5.0  &&  fabs((float)ang2)<=1 )  )  
	// if(  *(SANG+1)>5.0     ||     ( *(SANG+1)<5.0  &&  fabs((float)ang2)<=max_ang2/2   && fabs((float)ang1)<=max_ang1/2  &&  fabs((float)ang3)<=max_ang3/2 ) )   
            	{    
	       	   	for(k3=0; k3<nx;  k3++)
            	     	for(k4=0; k4<nx; k4++)
            	      	{   
                   	    		i=indx[k4+k3*nx];
            	         		j=indy[k4+k3*nx];
     
                        		if(abs(i)<=shift && abs(j)<=shift)
                        		{   ind=j+shift+(i+shift)*(2*shift+1)+k2*ind2;            
					    		

//if(*(SANG+1)<5 && k1==100)
//printf("k1=%d  xShift=%d yShift=%d ang1_rot=%d ang2_rot=%d ang3_rot=%d corr=%10.8e \n",k1, i, j,ang1,ang2,ang3,corr_image_total[ind]);

						if(max< corr_image_total[ind])
					    		{	x_shift=-i;
								y_shift=-j;
								ang1_rot=ang1;
								ang2_rot=ang2;
								ang3_rot=ang3;
								max=corr_image_total[ind];
						//	 	if(k1==800   )
						//	 		printf("k1=%d  xShift=%d yShift=%d ang1_rot=%d ang2_rot=%d ang3_rot=%d max_ang=%d       corr=%10.8e \n",k1, x_shift, y_shift,(int)ang1_rot,(int)ang2_rot,(int)ang3_rot,max_ang1,max);
 
					   		}
                         		}  
		           }
		}
	        k2++;      
}
 
 if(  k1==800 && Loop>1)
 printf("max=%f      shifts=%d %d      angles=%f   %f   %f     start=%d  end=%d  \n", max, x_shift, y_shift,  ang1_rot, ang2_rot, ang3_rot, start, end);	 
 
//   printf(" k1=%d  44444 in thread max=%f  gama=%f  sigma=%f \n", k1,max,gama,*sigma);
   fflush(stdout);

}

	   else
	   {		x_shift=0;
			y_shift=0;
			ang1_rot=0.0;
			ang2_rot=0.0;
			ang3_rot=0.0;
			max=1;
	   }
    




         	       
  
  //     total_corr+=max;
		 
		 /*  Insert a particle (shifted and rotated) into different projections     */
 

 	  if((max>1.0e-1 && Loop>1) || Loop==1)
	  {	

		//	total_corr+=max;
 


         		 /*  shift particle   in Fourier space 
			for(i=0;i<nx;i++)
            		for(j=0;j<nx;j++)
             		{    
                		//  m=(int)(sqrtf((i-nx/2)*(i-nx/2)*1.0+(j-nx/2)*(j-nx/2)*1.0));
                          
                		//  if(m>=rmax1 && m<rmax2)
                  		{   
                      		phas=-2*pi*(float)((i+nx/2)*x_shift+(j+nx/2)*y_shift)/(float)nx;
                                                   
                      		st2=B[j+i*nx]*cos(phas)-B[j+i*nx+nx*nx]*sin(phas);
                      		st3=B[j+i*nx]*sin(phas)+B[j+i*nx+nx*nx]*cos(phas);           
                      
				  		B[j+i*nx]=st2;
				  		B[j+i*nx+nx*nx]=st3;                  
			    		}
              		}
 				*/


			//  shift and mask  in real space
			 
			 for(i=0; i<nx; i++)
				for(j=0;j<nx;j++)
					SFimage[j+i*nx]=image[j+i*nx+k1*nx*nx];

			 translate(nx,nx,x_shift, y_shift, SFimage, SFimage1);

			 mask(realcell_x1_common-14,realcell_y1_common-14,nx,nx,SFimage1);
			 translate(nx, nx, nx/2, nx/2,SFimage1,SFimage);   

			 for(i=0; i<nx; i++)
        	  	 	for(j=0; j<nx; j++)
               		  	{ 	in[j+i*nx][0]=SFimage[j+i*nx]*powf(-1.0,i+j)/nx;  
                    		 	in[j+i*nx][1]=0;
                		 }

	 		fftwf_execute(p1);



			if(Loop>1)
			{

				//	max_max=exp(-10*acos(max/10000)+10);

				max_max=1/(acos(max/100)+3.1415927);

	 			total_corr+=1; // max;
				for(i=0;i<nx;i++)  
	      			for(j=0;j<nx;j++)
	        		{   
	             			B[j+i*nx]=out[j+i*nx][0];  
                  			B[j+i*nx+nx*nx]= out[j+i*nx][1];    
             			}
  if(k1==800   )
  printf("Loop=%d   k1= %d    xS=%d yS=%d an1=%d an2=%d an3=%d max=%10.4e   max_max=%10.4e  \n",Loop, k1, x_shift, y_shift,(int)ang1_rot,(int)ang2_rot,(int)ang3_rot,max, max_max);
 getchar();
			}
			else
			{	total_corr+=max;		
				max_max=1;
				for(i=0;i<nx;i++)  
	      			for(j=0;j<nx;j++)
	        		{   
	             			B[j+i*nx]=out[j+i*nx][0];  
                  			B[j+i*nx+nx*nx]= out[j+i*nx][1];     
             			}
			}


 	
            	angles[0]=(ang1_rot+(*(SANG)));  angles[1]=(ang2_rot+(*(SANG+1)));  angles[2]=(ang3_rot +(*(SANG+2)));
             
            	insert3D(B, angles, nx,nx,nx, new_re_refer, new_im_refer, normal, count,max_max); 


/*
			angles[0]+=90;   

			insert3D(B, angles, nx,nx,nx, new_re_refer, new_im_refer, normal, count,max_max); 

			angles[0]+=90;   

			insert3D(B, angles, nx,nx,nx, new_re_refer, new_im_refer, normal, count,max_max); 

			angles[0]+=90;   

			insert3D(B, angles, nx,nx,nx, new_re_refer, new_im_refer, normal, count,max_max); 
*/


			 
			if(fmod(k1*1.0,2.0)>=0.5)
					insert3D(B, angles, nx,nx,nx, new_re_refer1, new_im_refer1, normal1, count1,max_max);
			else
					insert3D(B, angles, nx,nx,nx, new_re_refer2, new_im_refer2, normal2, count2, max_max);  
		 
	}                    
                         
}    // end of images per thread  k1-Loop        
                       
    
              
               /* Return the parameters to the master thread*/     
               
            

  
             pthread_mutex_lock(&lock5); 
                *(new_par)+=new_dev_sigma;
                *(new_par+1)+=new_dev_x;
                *(new_par+2)+=new_dev_y;
                *(new_par+3)+=new_dev_phi;
                *(new_par+4)+=new_dev_theta;
                *(new_par+5)+=new_dev_psi;
                *(new_par+6)+=new_sigma_phi;
                *(new_par+7)+=new_sigma_theta;
                *(new_par+8)+=new_sigma_psi;
      //          for(i=0;i<nx/2;i++)
     //           if(num[i]>0)
                   *(new_par+9)+=new_sigma[0]; 

		       *(new_par+10)+=total_corr;
             pthread_mutex_unlock(&lock5);              
                    
             pthread_mutex_lock(&lock6);     
                fftwf_free(in); 
                fftwf_free(out);
                fftwf_destroy_plan(p1);
                fftwf_destroy_plan(p2); 
             pthread_mutex_unlock(&lock6);     
            
             
            
             
             
             
             
             
              free(new_sigma);
              free(B);
              free(indx);
              free(indy);
              free(num);
              free(weight);
              free(angles);
              free(corr);
              free(corr_image);
              free(corr_image_total);
              
    //          printf("END OF THREAD \n");
        
            pthread_exit(NULL);      
                           
 }               
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                    
                        
