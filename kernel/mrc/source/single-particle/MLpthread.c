 
 
 void *MLthread(void* data)
 {          
                   
              int Gau=2;
              int  rmax1, rmax2;
              int shift,Num_angles, Num_images;
 
             my_struct *pr;
             pr=(my_struct *)data;
             
             int ang1,ang2,ang3,ind,i,j,i1,j1,k,m,k1,k2,k3,k4, start,end,  ind1,ind2,ind3,nx,Loop=0;
             int *num, *indx,*indy;
             
             float *pdf, *re_ref, *im_ref, *pow_refer, *re, *im,*re_CTF, *im_CTF, *pow_image, *SANG, *sigma, *new_re_refer, *new_im_refer,*new_re_refer_CTF, *new_im_refer_CTF;
             float *normal,*normal_CTF, *new_par;
             float dev_x,dev_y,dev_phi,dev_theta,dev_psi;
             
             float new_dev_sigma, new_dev_x, new_dev_y;
             float new_dev_phi, new_sigma_phi, new_dev_theta, new_sigma_theta, new_dev_psi, new_sigma_psi;
             float *new_sigma,  *B, dev_sigma_change,t1;
             float *corr, *weight, *angles, *corr_image, *corr_image_total;
         
             float gama=0.0, gama1,st1,st2, st3, max,phas; 
 
             
             /*  Get the data or address from the master thread*/   
     //      pthread_mutex_lock(&lock2); 
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
             re_CTF=(*pr).re_samp_CTF;
	     im_CTF=(*pr).im_samp_CTF;
	     pow_image=(*pr).pow_samp;
	     SANG=(*pr).angle;
	     sigma=(*pr).sigma;
	     new_re_refer=(*pr).new_re_refer;
	     new_im_refer=(*pr).new_im_refer;
	     normal=(*pr).normal;
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
                        
             corr=(float *)malloc(sizeof(float)*nx);
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
                     
 
               /*   Calculate cross correlation of the particles and the reference in Fourier space  */
                    max=-1.0e20;  
                    k2=0;
                    for(ang1=-max_ang1;ang1<max_ang1;ang1+=step_angle1)
                      for(ang2=-max_ang2;ang2<max_ang2;ang2+=step_angle2)
                         for(ang3=-max_ang3;ang3<max_ang3;ang3+=step_angle3)
                         if(Loop<0)
                         {
                             for(k3=-shift; k3<=shift;  k3++)
                               for(k4=-shift; k4<=shift; k4++)
                               {   
                                    ind=k4+shift+(k3+shift)*(2*shift+1)+k2*ind2; 
                                    corr_image_total[ind]=0.0;

                                    for(m=0; m<nx; m++)
                                             corr_image[ind+m*ind3]=0.0; 
                            
                                    for(i=0;i<nx;i++)
                                      for(j=0;j<nx;j++)
                                      {    
                                              m=(int)(sqrtf((i-nx/2)*(i-nx/2)*1.0+(j-nx/2)*(j-nx/2)*1.0));
                           
                                             if(m>=rmax1 && m<rmax2)
                                              {   
                                                   phas=-2*pi*(float)(i*k3+j*k4)/(float)nx;
                                                   
                                                   st2=(*(re_ref+j+i*nx+k2*nx*nx))*cos(phas)-(*(im_ref+j+i*nx+k2*nx*nx))*sin(phas);
                                                   st3=(*(re_ref+j+i*nx+k2*nx*nx))*sin(phas)+(*(im_ref+j+i*nx+k2*nx*nx))*cos(phas);             
                    
                    
                                                   t1=(*(re+j+i*nx+k1*nx*nx))*st2+(*(im+j+i*nx+k1*ind1))*st3;
                                                   corr_image[ind+m*ind3]+=t1; 
                                                   corr_image_total[ind]+=(t1/powf(*(sigma+m),2.0));       
                                                   
                                                               
                                              }
                                       }
                               
                                    if( corr_image_total[ind]>=max) 
                                       max= corr_image_total[ind];
                                         
                                 }
 
                              k2++;
                         }
                         else
                         {               		 
                              for(i=0;i<nx;i++)
        			for(j=0;j<nx;j++)
          			{      m=(int)(sqrtf((i-nx/2)*(i-nx/2)*1.0+(j-nx/2)*(j-nx/2)*1.0));
          			       if(m>=rmax1 && m<rmax2)
          			        {	                  
          				   out[j+i*nx][0]=(*(re+j+i*nx+k1*nx*nx))*(*(re_ref+j+i*nx+k2*nx*nx))+(*(im+j+i*nx+k1*nx*nx))*(*(im_ref+j+i*nx+k2*nx*nx));
	      				  out[j+i*nx][1]= -(*(re+j+i*nx+k1*nx*nx))*(*(im_ref+j+i*nx+k2*nx*nx))+(*(im+j+i*nx+k1*nx*nx))*(*(re_ref+j+i*nx+k2*nx*nx));
       					} 
	      				else
	      				{	out[j+i*nx][0]=0;
	      					out[j+i*nx][1]=0;
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
                                        corr_image_total[ind]=in[k4+k3*nx][0]*powf(-1.0,k3+k4); 
                                         			
                                        if( corr_image_total[ind]>=max) 
                                           max= corr_image_total[ind];
  	
	            		      }	
	            	           }
	            		         	
	            		 k2++;            		     
	            	    }	 
                          
   printf(" k1=%d  333333333 in thread \n", k1);
  
	             gama=0;	
	             ind=0;	 
	             k2=0;      
		     for(ang1=-max_ang1;ang1<max_ang1;ang1+=step_angle1)
                      for(ang2=-max_ang2;ang2<max_ang2;ang2+=step_angle2)
                        for(ang3=-max_ang3;ang3<max_ang3;ang3+=step_angle3)
                         {
		       		 for(k3=0; k3<nx;  k3++)
                          		for(k4=0; k4<nx; k4++)
                           		{   
                                   	    i=indx[k4+k3*nx];
                                   	    j=indy[k4+k3*nx];
                                   	    if(abs(i)<=shift && abs(j)<=shift)
                                   	    {      
                                   		ind=j+shift+(i+shift)*(2*shift+1)+k2*ind2;                
                                         	weight[k4+k3*nx+k2*nx*nx]=exp(corr_image_total[ind]-max)/powf(*(sigma),2.0)*(*(pdf+k4+k3*nx+k2*nx*nx)); 
                          		        gama=gama+weight[k4+k3*nx+k2*nx*nx];       
                                   	     }                   		
		             		  } 
		             		
		             	   k2++;		            
		            }
		
   
                      k2=0;
 
		      for(ang1=-max_ang1;ang1<max_ang1;ang1+=step_angle1)
                      for(ang2=-max_ang2;ang2<max_ang2;ang2+=step_angle2)
                         for(ang3=-max_ang3;ang3<max_ang3;ang3+=step_angle3)
                         {    
		             for(k3=0; k3<nx;  k3++)
                          	for(k4=0; k4<nx; k4++)
                          	 {   
                          		i=indx[k4+k3*nx];
                          		j=indy[k4+k3*nx];
     
                          		if(abs(i)<=shift && abs(j)<=shift)
                          	        {   ind=j+shift+(i+shift)*(2*shift+1)+k2*ind2;       
                          		    weight[k4+k3*nx+k2*nx*nx]=weight[k4+k3*nx+k2*nx*nx]/(1.0e-40+gama);  
                          	        }
	            			else
	            			    weight[k4+k3*nx+k2*nx*nx]=0;   
   
		                  }
		             k2++;      
		          }
   
       	       
	             /*    Update the translation parameters    */
		     for(k2=0;k2<nx;k2++)
                            corr[k2]=0.0;
  
    
                   k2=0;
                   for(ang1=-max_ang1; ang1<max_ang1; ang1+=step_angle1)
                      for(ang2=-max_ang2; ang2<max_ang2; ang2+=step_angle2)
                         for(ang3=-max_ang3; ang3<max_ang3; ang3+=step_angle3)	
                         { 
                             for(k3=0;k3<nx;k3++)    //  translation in x
                          	 for(k4=0;k4<nx;k4++)  // translation in y
		                 {   
		                      i=indx[k4+k3*nx];
                                      j=indy[k4+k3*nx];
                                      if(abs(i)<=shift && abs(j)<=shift)  
                                      {    
                                              	ind=j+shift+(i+shift)*(2*shift+1)+k2*ind2; 
              
               
                                              	
                                                new_dev_sigma+=((powf(indx[k4+k3*nx]-dev_x,Gau)+powf(indy[k4+k3*nx]-dev_y,Gau))*weight[k4+k3*nx+k2*nx*nx]);  
				                new_dev_x+=(indx[k4+k3*nx]*weight[k4+k3*nx+k2*nx*nx]);
			                        new_dev_y+=(indy[k4+k3*nx]*weight[k4+k3*nx+k2*nx*nx]); 
				  
				  
				                new_dev_phi+=(ang1*weight[k4+k3*nx+k2*nx*nx]);
				                new_sigma_phi+=(powf(ang1-dev_phi,Gau)*weight[k4+k3*nx+k2*nx*nx]); 
				  
				                new_dev_theta+=(ang2*weight[k4+k3*nx+k2*nx*nx]);
				                new_sigma_theta+=(powf(ang2-dev_theta,Gau)*weight[k4+k3*nx+k2*nx*nx]); 
				
				                new_dev_psi+=(ang3*weight[k4+k3*nx+k2*nx*nx]);
				                new_sigma_psi+=(powf(ang3-dev_psi,Gau)*weight[k4+k3*nx+k2*nx*nx]); 
				
               
                                                st1=0;
                                                for(m=0; m<nx;  m++)
                                                 if(num[m]>0  )
                                                       st1+=(*(pow_refer+k2+m*Num_angles));
                                                       
                                                         
				               corr[0]+=((st1-2*corr_image_total[ind])*weight[k4+k3*nx+k2*nx*nx]);
				 
				              
                                        }
			          }
			    k2++;  
			}
 	  
 	
	            //        Update the noise std     
               //   for(m=0; m<nx; m++)  
               //    if(num[m]>0 )
               //        new_sigma[0]+=(*(pow_image+m+k1*nx/2));
                          
                   new_sigma[0]+=(*(pow_image+k1*nx/2)+corr[0]);
           


		 
		 //   Insert a particle into different projections by probability      
                     k2=0;   
                     for(ang1=-max_ang1;ang1<max_ang1;ang1+=step_angle1)
                      for(ang2=-max_ang2;ang2<max_ang2;ang2+=step_angle2)
                        for(ang3=-max_ang3;ang3<max_ang3;ang3+=step_angle3)
                         {       
		              for(i=0;i<nx;i++)
      		                for(j=0;j<nx;j++)
      		                {   
		  	            in[j+i*nx][0]=weight[j+i*nx+k2*nx*nx]*powf(-1.0,i+j); 
      		                    in[j+i*nx][1]=0.0; 
	                        }
                          
      		              fftwf_execute(p1);
      		 
		              for(i=0;i<nx;i++)  
	                        for(j=0;j<nx;j++)
	                        {
	                            B[j+i*nx]=(*(re+j+i*nx+k1*nx*nx))*out[j+i*nx][0]+(*(im+j+i*nx+k1*nx*nx))*out[j+i*nx][1];  
                                    B[j+i*nx+nx*nx]= -(*(re+j+i*nx+k1*nx*nx))*out[j+i*nx][1]+(*(im+j+i*nx+k1*nx*nx))*out[j+i*nx][0];   
                            	 }  
                       
                              angles[0]=(ang1+(*(SANG)));  angles[1]=(ang2+(*(SANG+1)));  angles[2]=(ang3+(*(SANG+2)));
                        
                              insert3D(B, angles, nx,nx,nx, new_re_refer, new_im_refer, normal);  

                              for(i=0;i<nx;i++)  
	                        for(j=0;j<nx;j++)
	                        {
	                           B[j+i*nx]=(*(re_CTF+j+i*nx+k1*nx*nx))*out[j+i*nx][0]+(*(im_CTF+j+i*nx+k1*nx*nx))*out[j+i*nx][1];  
                                   B[j+i*nx+nx*nx]= -(*(re_CTF+j+i*nx+k1*nx*nx))*out[j+i*nx][1]+(*(im_CTF+j+i*nx+k1*nx*nx))*out[j+i*nx][0];   
                            	}  
       
                              insert3D(B, angles, nx,nx,nx, new_re_refer_CTF, new_im_refer_CTF, normal_CTF);    

                              k2++;    		         
                         }
                        
                        
                         
           }    // end of images per thread          
                       
    
              
               /* Return the parameters to the master thread*/     
               
            

  
     //         pthread_mutex_lock(&lock5); 
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
      //           pthread_mutex_unlock(&lock5);              
                    
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
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                    
                        
