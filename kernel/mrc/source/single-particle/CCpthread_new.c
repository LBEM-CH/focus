/* 		Align the particles 
		1.  using traditional cross-correlation calculated by fast Fourier transform 
		2.  or weighted cross_correlation

		implemented in multi-threaded programs.

*/ 


    	 

 

 
#include "ccp.c"
#include "simplex.c"
 
 void *CCthread(void* data)
 {          
            
    int Gau=2;
    int  rmax1, rmax2, flag;
    int shift,Num_angles, Num_images;
 
    my_struct *pr;
    pr=(my_struct *)data;
             
    int ang1,ang2,ang3,ind,i,j,i1, j1,  k,m,n,k1,k2,k3,k4, start,end,  ind1,ind2,ind3,nx,Loop, ii, jj;
    int *num, *indx,*indy, *count, *count1,*count2;
             
    float *pdf, *re_ref, *im_ref,  *pow_refer, *re, *im,*re_CTF, *im_CTF, *pow_image, *SANG, *sigma, *image;
    float *new_re_refer, *new_im_refer, *new_re_refer1, *new_im_refer1, *new_re_refer2, *new_im_refer2, *new_re_refer_CTF, *new_im_refer_CTF;
    float *normal, *normal1, *normal2, *normal_CTF, *new_par, *SFimage, *SFimage1;
    float dev_x,dev_y,dev_phi,dev_theta,dev_psi, wgt, max_corr, y_center, diff;
       
    
    float new_dev_sigma, new_dev_x, new_dev_y;
    float new_dev_phi, new_sigma_phi, new_dev_theta, new_sigma_theta, new_dev_psi, new_sigma_psi;
    float *new_sigma,  *B, dev_sigma_change,t1;
    float *corr, *weight,  *corr_image, *corr_image_total, *amp1, *amp2 ;
    float *refer;
 
 
    float tmp;
    float *angles,  *y, *rot_angles;
    int *shift_xy;
   
    float gama=0.0, gama1,st1,st2, st3,st4, max,phas, max_max; 

    double total_corr=0.0;
             
    /*  Get the data or address from the master thread*/   
    //      pthread_mutex_lock(&lock2); 

    fftwf_plan  p2_fw, p2_bw;

    p2_fw=(*pr).dft2_fw;
    p2_bw=(*pr).dft2_bw;
    refer=(*pr).ref;
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

 

   Num_images=end-start;	      	     
 
   int  Npar=(max_ang1*2/step_angle1)+1,  NSpar=4;
   Npar=Npar*Npar*Npar;
 

    rot_angles=(float *)calloc(3,sizeof(float));
    angles=(float *)calloc(Npar*3, sizeof(float));
    shift_xy=(int *)calloc(Npar*2, sizeof(int));
    y=(float *)calloc(Npar, sizeof(float));

    float *angles_4=(float *)calloc(NSpar*3,sizeof(float)), *y_4=(float *)calloc(NSpar,sizeof(float));
    int *shift_xy_4=(int *)calloc(NSpar*2, sizeof(int));	


    new_sigma=(float *)calloc(nx/2,sizeof(float));
            
    indx=(int *)malloc(sizeof(int)*nx*nx);
    indy=(int *)malloc(sizeof(int)*nx*nx);
    num=(int *)malloc(sizeof(int)*nx);
       
    weight=(float *)malloc(sizeof(float)*nx*nx*Num_angles); 
    B=(float *)calloc(nx*nx*2,sizeof(float));

    SFimage=(float *)calloc(nx*nx,sizeof(float));
    SFimage1=(float *)calloc(nx*nx,sizeof(float));

    corr=(float *)calloc(nx/2, sizeof(float));	               
   
    corr_image=(float *)malloc(sizeof(float)*nx*Num_angles*(2*shift+1)*(2*shift+1));
    corr_image_total=(float *)malloc(sizeof(float)*Num_angles*(2*shift+1)*(2*shift+1));
    
   amp1=(float *)calloc(nx/2, sizeof(float));
    amp2=(float *)calloc(nx/2, sizeof(float));      
    corr=(float *)calloc(nx/2, sizeof(float));	        
       
    fftwf_complex *in,*out;	 

      
    pthread_mutex_lock(&lock3); 
    	in=(fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*nx*nx);
    	out=(fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*nx*nx); 
        
//    	p1=fftwf_plan_dft_2d(nx,nx,in,out, FFTW_FORWARD,FFTW_ESTIMATE);
//    	p2=fftwf_plan_dft_2d(nx,nx,out,in,FFTW_BACKWARD,FFTW_ESTIMATE);
    pthread_mutex_unlock(&lock3);     
      
    
    

/*
float max11;
max11=-1.0e20;
for(i=0;i<nx;i++)
for(j=0;j<nx;j++)
for(k=0;k<nx;k++)
if(max11< powf(refer[k+j*nx+i*nx*nx],2.0)+powf(refer[k+j*nx+i*nx*nx+nx*nx*nx],2.0))
max11= powf(refer[k+j*nx+i*nx*nx],2.0)+powf(refer[k+j*nx+i*nx*nx+nx*nx*nx],2.0);
printf(" INCCPthread   Simplex y=%f  \n",max11);    fflush(stdout); 
*/

        
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
	
 
      max_corr=-1.0e20;  
     for(k1=start;k1<end;k1++)
     {     
     	 
        /*   Calculate cross correlation of the particles and the reference in Fourier space  */  

	if(Loop>1)
	{ 

/*		Calculate weighted cross correlation 

        k2=0;
        for(ang1=-max_ang1;ang1<=max_ang1;ang1+=step_angle1)
          for(ang2=-max_ang2;ang2<=max_ang2;ang2+=step_angle2)
            for(ang3=-max_ang3;ang3<=max_ang3;ang3+=step_angle3)
            {       
			y[k2]=-1.0e20;
			angles[0+k2*3]=ang1+(*(SANG));
			angles[1+k2*3]=ang2+(*(SANG+1));
			angles[2+k2*3]=ang3+(*(SANG+2));      
  		  	max=-1.0e20;		     	 
    		     	for(k3=-shift; k3<=shift;  k3++)
                   	for(k4=-shift; k4<=shift; k4++)
                 	{   		

					ind=k4+shift+(k3+shift)*(2*shift+1)+k2*ind2; 
                      			corr_image_total[ind]=0.0;
  	
					for(m=0;m<nx/2;m++)
					{	amp1[m]=0.0;
						amp2[m]=0.0;
						corr[m]=0.0;
					}


				  	for(i=0;i<nx;i++)
        	  	  	    		for(j=0;j<nx;j++)
          	 	    			{  	m=(int)(sqrtf((i-nx/2)*(i-nx/2)*1.0+(j-nx/2)*(j-nx/2)*1.0));
          	       	   				if(m>=rmax1 && m<=rmax2)
                    	  				{ 	 
								phas=-2*pi*(float)(i*k3+j*k4)/(float)nx;

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
							
						 	corr_image_total[ind]+=fabs(powf(st2,3.0))*wgt;

				  	}

					if(max<corr_image_total[ind])
					{	
							max=corr_image_total[ind];
							y[k2]=max;
							shift_xy[0+k2*2]=-k3;
							shift_xy[1+k2*2]=-k4;
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
          		 
               		 	for(i=0;i<nx;i++)
        	  		for(j=0;j<nx;j++)
          	  		{      m=(int)(sqrtf((i-nx/2)*(i-nx/2)*1.0+(j-nx/2)*(j-nx/2)*1.0));

				
          	       	  	//	 if(m>=rmax1 && m<rmax2)
          	         		 { 
						wgt=exp(-m*m/powf(rmax2*0.9,2.0));
          				      	out[j+i*nx][0]=re[j+i*nx+k1*nx*nx]*re_ref[j+i*nx+k2*nx*nx]+im[j+i*nx+k1*nx*nx]*im_ref[j+i*nx+k2*nx*nx];
	      				      	out[j+i*nx][1]= -re[j+i*nx+k1*nx*nx]*im_ref[j+i*nx+k2*nx*nx]+im[j+i*nx+k1*nx*nx]*re_ref[j+i*nx+k2*nx*nx]; 

						out[j+i*nx][0]*=wgt;
						out[j+i*nx][1]*=wgt;
			
       					  } 
	      				//  else
	      				//  {	out[j+i*nx][0]=0.0;
	      				//	out[j+i*nx][1]=0.0;
	      				//  }
	    		  	}	 
     			   
		     		fftwf_execute_dft(p2_bw, out, in );
 
				y[k2]=-1.0e20;
				angles[0+k2*3]=ang1+(*(SANG));
				angles[1+k2*3]=ang2+(*(SANG+1));
				angles[2+k2*3]=ang3+(*(SANG+2));

				// find the maximum CC
				max=-1.0e20;
    		     		for(k3=0; k3<nx;  k3++)
                 		for(k4=0; k4<nx; k4++)
                 		{   
                    			i=indx[k4+k3*nx];
                    			j=indy[k4+k3*nx];
        
                    			if(abs(i)<=shift && abs(j)<=shift)
                    			{
                    				ind=j+shift+(i+shift)*(2*shift+1)+k2*ind2; 	


						corr_image_total[ind]=0;
						k=0;
						for(m=k3-2; m<=k3+2;m++)
							for(n=k4-2; n<=k4+2;n++)
							if(m>=0 &&  m<nx && n>=0 && n<nx)
							{	corr_image_total[ind]+=in[n+m*nx][0]*powf(-1.0,n+m)/(nx)*exp(-(powf(k3-m,2.0)+powf(k4-n,2.0))/1); 
								k++;
							}
	
						 corr_image_total[ind]/=k;

                         			// corr_image_total[ind]=in[k4+k3*nx][0]*powf(-1.0,k3+k4)/(nx); 
						if(max<corr_image_total[ind])
						{	
							max=corr_image_total[ind];
							y[k2]=max;
							shift_xy[0+k2*2]=-i;
							shift_xy[1+k2*2]=-j;
						}
	              			}	
	           		}	
	         	
	       	     		k2++;            		     
  	  		}
 

		if(max_corr<max)
			max_corr=max;	 

   
	}   // for Loop >1 case
 

	//    Insert into the 3D volume

          for(i=0;i<nx;i++)  
	  	for(j=0;j<nx;j++)
	  	{   
	  		B[j+i*nx]=re[j+i*nx+k1*nx*nx];  
                	B[j+i*nx+nx*nx]= im[j+i*nx+k1*nx*nx];     
             	}
  
	   if(Loop==1)
	   {	
			total_corr+=1;		
			max_max=1;


			for(i=0;i<nx;i++)
                            	for(j=0;j<nx;j++)
                            	{    	phas=-2*pi*((i+nx/2)*nx/2+(j+nx/2)*nx/2)/nx;
                                	B[j+i*nx]=re[j+i*nx+k1*nx*nx]*cos(phas)- im[j+i*nx+k1*nx*nx]*sin(phas);
                                   	B[j+i*nx+nx*nx]=re[j+i*nx+k1*nx*nx]*sin(phas)+ im[j+i*nx+k1*nx*nx]*cos(phas);
                            	}

				
			rot_angles[0]=((*(SANG)));  rot_angles[1]=((*(SANG+1)));  rot_angles[2]=((*(SANG+2)));
             
            		insert3D(B, rot_angles, nx,nx,nx, new_re_refer, new_im_refer, normal, count,max_max); 
 
			if(fmod(k1*1.0,2.0)>=0.5)
				insert3D(B, angles, nx,nx,nx, new_re_refer1, new_im_refer1, normal1, count1,max_max);
			else
				insert3D(B, angles, nx,nx,nx, new_re_refer2, new_im_refer2, normal2, count2, max_max);  	
		
	   }
	   else 
	   {	   
 
			//   sort angles by cc values
    			for(i=0;  i<Npar-1;  i++)
			   for(j=Npar-1;  j>i;  j--)
				if(y[j]>y[j-1])
				{	for(k=0;  k<3;  k++)
					{	
						tmp=angles[k+j*3];
						angles[k+j*3]=angles[k+(j-1)*3];
						angles[k+(j-1)*3]=tmp;
					}
		
 
					for(k=0;k<2;k++)
					{	
						tmp=shift_xy[k+j*2];
						shift_xy[k+j*2]=shift_xy[k+(j-1)*2];
						shift_xy[k+(j-1)*2]=(int)tmp;
					}
		
					tmp=y[j];
					y[j]=y[j-1];
					y[j-1]=tmp;
				}

			k2=0;
			k=(floor)(Npar/4);
		        for(i=0; i<NSpar; i++)
			{ 	for(j=0;j<3;j++)	
					angles_4[j+i*3]=angles[j+k2*3];
				for(j=0;j<2;j++)
					shift_xy_4[j+i*2]=shift_xy[j+k2*2];
				
				y_4[i]=y[k2];
				k2=k2+k;			
		    	}
 

 
			k3=0; 
			y_center=0;
			for(i=0; i<NSpar; i++)
				y_center+=y_4[i];
			y_center/=NSpar;

			diff=0;
			for(i=0; i<NSpar; i++)
				diff+=powf(y_center-y_4[i],2.0);
			diff=sqrt(diff/NSpar);
			

/*
		   	 // simplex algorithm
 	 		while(y_4[0]>1.0e-1  && diff>1.0e-2 && k3<100 )
		   	{ 	k3++;

 
				//  call simplex
				flag=simplex(SANG, nx,refer,B,angles_4,shift_xy_4,y_4,NSpar, p2_fw, p2_bw);
 

				//   sort angles by cc values
    				for(i=0; i<NSpar-1;  i++)
				      for(j=NSpar-1;  j>i;  j--)
					if(y_4[j]>y_4[j-1])
					{	for(k=0;k<3;k++)
						{	
							tmp=angles_4[k+j*3];
							angles_4[k+j*3]=angles_4[k+(j-1)*3];
							angles_4[k+(j-1)*3]=tmp;
						}
		
						for(k=0;k<2;k++)
						{	
							tmp=shift_xy_4[k+j*2];
							shift_xy_4[k+j*2]=shift_xy_4[k+(j-1)*2];
							shift_xy_4[k+(j-1)*2]=(int)tmp;
						}
		
						tmp=y_4[j];
						y_4[j]=y_4[j-1];
						y_4[j-1]=tmp;
					}


					y_center=0;
					for(i=0; i<NSpar; i++)
						y_center+=y_4[i];
					y_center/=NSpar;

					diff=0;
					for(i=0; i<NSpar; i++)
						diff+=powf(y_center-y_4[i],2.0);
					diff=sqrt(diff/NSpar);


 
if(  k1==100 && Loop>1)	
 printf("k1=%d  k3=%d    angles=%f %f %f      %f %f %f    %f %f %f    shift=%d %d    y=%f   %f   %f    %f  flag=%d   diff=%f   \n",k1, k3, angles_4[0]-*(SANG), angles_4[1]-*(SANG+1), angles_4[2]-*(SANG+2), angles_4[3]-*(SANG), angles_4[4]-*(SANG+1), angles_4[5]-*(SANG+2), angles_4[6]-*(SANG), angles_4[7]-*(SANG+1), angles_4[8]-*(SANG+2),  shift_xy_4[0], shift_xy_4[1], y_4[0], y_4[1], y_4[2], y_4[3], flag, diff );   
fflush(stdout); 
 
		    	}

*/



		 //	if(y_4[0]>1 && y_4[1]>0 && y_4[2]>0  )
	  	    	{	

 
				for(i=0; i<nx; i++)
					for(j=0;j<nx;j++)
						SFimage[j+i*nx]=image[j+i*nx+k1*nx*nx];

		 		 translate(nx,nx,shift_xy_4[0], shift_xy_4[1], SFimage, SFimage1);

				 mask(realcell_x1_common-14,realcell_y1_common-14,nx,nx,SFimage1);
				 translate(nx, nx, nx/2, nx/2,SFimage1,SFimage);   

				 for(i=0; i<nx; i++)
        	  	 		for(j=0; j<nx; j++)
               		  		{ 	in[j+i*nx][0]= SFimage[j+i*nx]*powf(-1.0,i+j)/nx;  
                    		 		in[j+i*nx][1]=0;
                			 }
 
	 	 		fftwf_execute_dft(p2_fw, in, out);

		 
				 max_max=-acos(y_4[0]/2000)+3.1415926/2;

				//	max_max=1/(acos(max/100)+3.1415927);


if(  k1==100 && Loop>1)	
 printf("k1=%d  k3=%d    angles=%f %f %f      %f %f %f    %f %f %f    shift=%d %d    y=%f   %f   %f    %f   max_max=%f   \n",k1, k3, angles_4[0]-*(SANG), angles_4[1]-*(SANG+1), angles_4[2]-*(SANG+2), angles_4[3]-*(SANG), angles_4[4]-*(SANG+1), angles_4[5]-*(SANG+2), angles_4[6]-*(SANG), angles_4[7]-*(SANG+1), angles_4[8]-*(SANG+2),  shift_xy_4[0], shift_xy_4[1], y_4[0], y_4[1], y_4[2], y_4[3], max_max );    fflush(stdout); 



				max_max=exp(max_max);


	 		 	total_corr+=y_4[0]; // max[k1*3+k2];
				for(i=0;i<nx;i++)  
	      			for(j=0;j<nx;j++)
	        		{   
	             			B[j+i*nx]=out[j+i*nx][0];  
                  			B[j+i*nx+nx*nx]= out[j+i*nx][1];    
             			}
   
            	        	rot_angles[0]=angles_4[0];  rot_angles[1]=angles_4[1]; rot_angles[2]=angles_4[2]; 
             
            	       		insert3D(B, rot_angles, nx,nx,nx, new_re_refer, new_im_refer, normal, count,max_max); 
 
		  
/*
			angles[2]+=90;   

			insert3D(B, rot_angles, nx,nx,nx, new_re_refer, new_im_refer, normal, count,max_max); 

			angles[2]+=90;   

			insert3D(B, rot_angles, nx,nx,nx, new_re_refer, new_im_refer, normal, count,max_max); 

			angles[2]+=90;   

			insert3D(B, rot_angles, nx,nx,nx, new_re_refer, new_im_refer, normal, count,max_max); 

*/		 









				if(fmod(k1*1.0,2.0)>=0.5)
					insert3D(B, rot_angles, nx,nx,nx, new_re_refer1, new_im_refer1, normal1, count1,max_max);
				else
					insert3D(B, rot_angles, nx,nx,nx, new_re_refer2, new_im_refer2, normal2, count2, max_max);  
		 
		   	}      //   end of IF 
 
	    	}      // end of IF   
     
                    
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
//                fftwf_destroy_plan(p1);
//                fftwf_destroy_plan(p2); 
             pthread_mutex_unlock(&lock6);     
            
    

              free(new_sigma);
              free(B);
              free(SFimage);
	      free(SFimage1);
              free(indx);
              free(indy);
              free(num);
              free(weight);
              free(corr);
              free(corr_image);
              free(corr_image_total);
  
		free(shift_xy);
		free(rot_angles);
		free(angles);
		free(y);
		free(shift_xy_4);
		free(angles_4);
		free(y_4);
 


  //       printf("END OF THREAD \n");    fflush(stdout);       
        
     //       pthread_exit(NULL);      
                           
 }               
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                    
                        
