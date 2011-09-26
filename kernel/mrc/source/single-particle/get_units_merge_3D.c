#define DS 1
#define RA 2
  
int get_units_merge_3D(int *num_images,int num_peaks,int tag, float *peak_x, float *peak_y, int sx,int sy,int realcell_x1,int realcell_y1, int realcell_x1_common, int realcell_y1_common, int realcell_x,int realcell_y,float *unbend_image1, float *unbend_image2, float *unbend_image3, float *Image1, float *Image2, float *Image3)
{   int k,i,j,m;
     double mean,devi_total,devi, wgt, mm;
     float *temp_unit_small, *temp_unit, *temp_unit_large, *temp;
      
     temp_unit_small=(float *)calloc(realcell_x1*realcell_y1,sizeof(float));
     temp_unit=(float *)calloc(realcell_x1_common*realcell_y1_common,sizeof(float));
     temp_unit_large=(float *)calloc(realcell_x*realcell_y,sizeof(float));  

	temp=(float *)calloc(realcell_x1*realcell_y1*RA*RA*4,sizeof(float));  


     /*  Get the standard deviation of the large image (CTF corrected) */
     mean=0; devi_total=0.0;
     for(i=0;i<sx;i++)
       for(j=0;j<sy;j++)
        mean+=unbend_image2[j+i*sy];

     mean/=(sx*sy);
     for(i=0;i<sx;i++)
       for(j=0;j<sy;j++)
          devi_total+=powf(unbend_image2[j+i*sy]-mean,2.0);
     devi_total/=(sx*sy);


//     printf("realcell_x=%d realcell_x_common=%d \n", realcell_x1, realcell_x1_common);


	fftwf_complex *in,*out, *in_common, *out_common;
     fftwf_plan p; 
     in=(fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*realcell_x1*realcell_y1);
     out=(fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*realcell_x1*realcell_y1); 
        
     in_common=(fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*realcell_x1_common*realcell_y1_common);
     out_common=(fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*realcell_x1_common*realcell_y1_common); 

     m=0; 
     for(k=tag;k<tag+num_peaks;k++) 
//     if(peak_x[k]-realcell_x1/2*DS>=0 && peak_x[k]+realcell_x1/2*DS<sx && peak_y[k]-realcell_y1/2*DS>=0 && peak_y[k]+realcell_y1/2*DS<sy ) 

     if(peak_x[k]-realcell_x1*RA>=0 && peak_x[k]+realcell_x1*RA<sx && peak_y[k]-realcell_y1*RA>=0 && peak_y[k]+realcell_y1*RA<sy ) 
     {  




	   //  Extract patches from micrograph 	                
        for(i=-realcell_x1/2;i<realcell_x1/2;i++)
	     for(j=-realcell_y1/2;j<realcell_y1/2;j++)   
		 temp_unit_small[j+realcell_y1/2+(i+realcell_x1/2)*realcell_y1]=unbend_image2[j*DS+(int)peak_y[k]+(i*DS+(int)peak_x[k])*sy];
 

 

        mean=0; devi=0.0;
        for(i=0;i<realcell_x1;i++)
          for(j=0;j<realcell_y1;j++)
            mean+=temp_unit_small[j+i*realcell_y1];

        mean/=(realcell_x1*realcell_y1);
          for(i=0;i<realcell_x1;i++)
            for(j=0;j<realcell_y1;j++)
              devi+=powf(temp_unit_small[j+i*realcell_y1]-mean,2.0);
        devi/=(realcell_x1*realcell_y1);
 	
                 
        if(devi>devi_total*0.02)
        {
		  //  PROCESS  CTF CORRECTED IMAGES
        	  //   padding in Fourier space, Scaling the  patch according to the  stepdigitizer     

		 mask(realcell_x1,realcell_y1,realcell_x1,realcell_y1,temp_unit_small); 
         	 for(i=0; i<realcell_x1; i++)
             for(j=0; j<realcell_y1; j++)
             {  in[j+i*realcell_x1][0]=temp_unit_small[j+i*realcell_x1]*powf(-1.0,i+j);  
                 in[j+i*realcell_x1][1]=0;
             }  
 
 	                  
		 p=fftwf_plan_dft_2d(realcell_x1,realcell_y1,in,out,FFTW_FORWARD,FFTW_ESTIMATE);
           fftwf_execute(p);
           fftwf_destroy_plan(p);  
                  
           for(i=0;i<realcell_x1_common; i++)
              for(j=0;j<realcell_y1_common;j++)
              {    out_common[j+i*realcell_y1_common][0]=0;
                   out_common[j+i*realcell_y1_common][1]=0;
              }
                 
           for(i=(realcell_x1_common-realcell_x1)/2 ;  i<(realcell_x1_common+realcell_x1)/2; i++)
             for(j=(realcell_y1_common-realcell_y1)/2; j<(realcell_y1_common+realcell_y1)/2; j++)
             { out_common[j+i*realcell_y1_common][0]=out[j-(realcell_y1_common-realcell_y1)/2+(i-(realcell_x1_common-realcell_x1)/2)*realcell_y1][0];
                out_common[j+i*realcell_y1_common][1]=out[j-(realcell_y1_common-realcell_y1)/2+(i-(realcell_x1_common-realcell_x1)/2)*realcell_y1][1];
        	   }
 	         
           p=fftwf_plan_dft_2d(realcell_x1_common,realcell_y1_common,out_common,in_common,FFTW_BACKWARD,FFTW_ESTIMATE);
           fftwf_execute(p);
           fftwf_destroy_plan(p);  
   


		//  padding in real space
		 for(i=0; i<realcell_x1_common; i++)
        	  for(j=0; j<realcell_y1_common; j++)
               temp_unit[j+i*realcell_x1_common]=in_common[j+i*realcell_x1_common][0]*powf(-1.0,i+j); 


	 
 	                	 
           mean=0;  
           for(i=0;i<realcell_x1_common;i++)
             for(j=0;j<realcell_y1_common;j++)
               mean+=temp_unit[j+i*realcell_y1_common];

           mean/=(realcell_x1_common*realcell_y1_common);    
   
		 for(i=0;i<realcell_x;i++)
	        for(j=0;j<realcell_y;j++)   
		     temp_unit_large[j+i*realcell_y]=mean;
	     	      
	      for(i=-realcell_x1_common/2;i<realcell_x1_common/2;i++)
	        for(j=-realcell_y1_common/2;j<realcell_y1_common/2;j++)   
			temp_unit_large[j+realcell_y/2+(i+realcell_x/2)*realcell_y]=temp_unit[j+realcell_y1_common/2+(i+realcell_x1_common/2)*realcell_y1_common];
		    


		mask(realcell_x1_common,realcell_y1_common,realcell_x,realcell_y,temp_unit_large);

 		for(i=0;i<realcell_x;i++)
	        for(j=0;j<realcell_y;j++)   
 			 Image2[j+i*realcell_y+m*realcell_x*realcell_y]=temp_unit_large[j+i*realcell_y];











 















             
		 //  PROCESS WHITENED IMAGES               
/*		 for(i=-realcell_x1/2;i<realcell_x1/2;i++)
	        for(j=-realcell_y1/2;j<realcell_y1/2;j++)   
		     temp_unit_small[j+realcell_y1/2+(i+realcell_x1/2)*realcell_y1]=unbend_image1[j*DS+(int)peak_y[k]+(i*DS+(int)peak_x[k])*sy]; 
*/

		 for(i=-realcell_x1*RA;i<realcell_x1*RA;i++)
	        for(j=-realcell_y1*RA;j<realcell_y1*RA;j++)   
		     temp[j+realcell_y1*RA+(i+realcell_x1*RA)*realcell_y1*RA*2]=unbend_image1[j*DS+(int)peak_y[k]+(i*DS+(int)peak_x[k])*sy]; 


mask(realcell_x1*RA*2-4, realcell_y1*RA*2-4, realcell_x1*RA*2, realcell_y1*RA*2, temp);

 
ctf_local(realcell_x1*RA*2,realcell_y1*RA*2, temp, (peak_x[k]-sx/2), -(peak_y[k]-sy/2));

		
 		for(i=-realcell_x1/2;i<realcell_x1/2;i++)
	        for(j=-realcell_y1/2;j<realcell_y1/2;j++)   
		     temp_unit_small[j+realcell_y1/2+(i+realcell_x1/2)*realcell_y1]= temp[j+realcell_y1*RA+(i+realcell_x1*RA)*realcell_y1*RA*2];

	  	 mask(realcell_x1,realcell_y1,realcell_x1,realcell_y1,temp_unit_small);
          
		       
		 //  padding in Fourier space, Scaling the  patch according to the  stepdigitizer       
		 for(i=0; i<realcell_x1; i++)
        	   for(j=0; j<realcell_y1; j++)
             	  {	   in[j+i*realcell_x1][0]=temp_unit_small[j+i*realcell_x1]*powf(-1.0,i+j);  
                  	   in[j+i*realcell_x1][1]=0;
                  }   
 	                  
	   	p=fftwf_plan_dft_2d(realcell_x1,realcell_y1,in,out,FFTW_FORWARD,FFTW_ESTIMATE);
           	fftwf_execute(p);
           	fftwf_destroy_plan(p);  
                  
           	for(i=0;i<realcell_x1_common; i++)
              		for(j=0;j<realcell_y1_common;j++)
             		{  	out_common[j+i*realcell_y1_common][0]=0;
                		out_common[j+i*realcell_y1_common][1]=0;
             		}
                 
           	for(i=(realcell_x1_common-realcell_x1)/2 ;  i<(realcell_x1_common+realcell_x1)/2; i++)
             		for(j=(realcell_y1_common-realcell_y1)/2; j<(realcell_y1_common+realcell_y1)/2; j++)
             		{ 	out_common[j+i*realcell_y1_common][0]=out[j-(realcell_y1_common-realcell_y1)/2+(i-(realcell_x1_common-realcell_x1)/2)*realcell_y1][0];
                		out_common[j+i*realcell_y1_common][1]=out[j-(realcell_y1_common-realcell_y1)/2+(i-(realcell_x1_common-realcell_x1)/2)*realcell_y1][1];
             		}


 	   	for(i=0;i<realcell_x1_common; i++)
                	for(j=0;j<realcell_y1_common;j++)
			{	mm=sqrtf(powf(i-realcell_x1_common/2,2.0)+powf(j-realcell_x1_common/2,2.0));
				wgt=exp(-mm*mm/powf(rmax2*0.9,2.0));

			  	out_common[j+i*realcell_y1_common][0]*=wgt;
			  	out_common[j+i*realcell_y1_common][1]*=wgt;
			}
 
 	                		




            	p=fftwf_plan_dft_2d(realcell_x1_common,realcell_y1_common,out_common, in_common, FFTW_BACKWARD,FFTW_ESTIMATE);
            	fftwf_execute(p);
            	fftwf_destroy_plan(p); 

 
		  // padding in real space   
		  for(i=0; i<realcell_x1_common; i++)
        	    for(j=0; j<realcell_y1_common; j++)
                 	temp_unit[j+i*realcell_x1_common]=in_common[j+i*realcell_x1_common][0]*powf(-1.0,i+j);       
 

           	mean=0;  
           	for(i=0;i<realcell_x1_common;i++)
             		for(j=0;j<realcell_y1_common;j++)
               		mean+=temp_unit[j+i*realcell_y1_common];

           	mean/=(realcell_x1_common*realcell_y1_common);    
   
	   	for(i=0;i<realcell_x;i++)
	       		for(j=0;j<realcell_y;j++)   
		     		temp_unit_large[j+i*realcell_y]=0.0; // mean;
	     	      
	      	for(i=-realcell_x1_common/2;i<realcell_x1_common/2;i++)
	        	for(j=-realcell_y1_common/2;j<realcell_y1_common/2;j++)   
				temp_unit_large[j+realcell_y/2+(i+realcell_x/2)*realcell_y]=temp_unit[j+realcell_y1_common/2+(i+realcell_x1_common/2)*realcell_y1_common];

		mask(realcell_x1_common,realcell_y1_common,realcell_x,realcell_y,temp_unit_large);

 		for(i=0;i<realcell_x;i++)
	       	 for(j=0;j<realcell_y;j++)   
 			 Image1[j+i*realcell_y+m*realcell_x*realcell_y]=temp_unit_large[j+i*realcell_y];











/*

		 for(i=-realcell_x1*RA;i<realcell_x1*RA;i++)
	        for(j=-realcell_y1*RA;j<realcell_y1*RA;j++)   
		     temp[j+realcell_y1*RA+(i+realcell_x1*RA)*realcell_y1*RA*2]=unbend_image3[j*DS+(int)peak_y[k]+(i*DS+(int)peak_x[k])*sy]; 


mask(realcell_x1*RA*2-4, realcell_y1*RA*2-4, realcell_x1*RA*2, realcell_y1*RA*2, temp);

 
ctf_local(realcell_x1*RA*2,realcell_y1*RA*2, temp, (peak_x[k]-sx/2), -(peak_y[k]-sy/2));

		
 		for(i=-realcell_x1/2;i<realcell_x1/2;i++)
	        for(j=-realcell_y1/2;j<realcell_y1/2;j++)   
		     temp_unit_small[j+realcell_y1/2+(i+realcell_x1/2)*realcell_y1]= temp[j+realcell_y1*RA+(i+realcell_x1*RA)*realcell_y1*RA*2];

	  	 mask(realcell_x1,realcell_y1,realcell_x1,realcell_y1,temp_unit_small);
          
		       
		 //  padding in Fourier space, Scaling the  patch according to the  stepdigitizer       
		 for(i=0; i<realcell_x1; i++)
        	   for(j=0; j<realcell_y1; j++)
             {   in[j+i*realcell_x1][0]=temp_unit_small[j+i*realcell_x1]*powf(-1.0,i+j);  
                  in[j+i*realcell_x1][1]=0;
             }   
 	                  
	   p=fftwf_plan_dft_2d(realcell_x1,realcell_y1,in,out,FFTW_FORWARD,FFTW_ESTIMATE);
           fftwf_execute(p);
           fftwf_destroy_plan(p);  
                  
           for(i=0;i<realcell_x1_common; i++)
             for(j=0;j<realcell_y1_common;j++)
             {  out_common[j+i*realcell_y1_common][0]=0;
                out_common[j+i*realcell_y1_common][1]=0;
             }
                 
           for(i=(realcell_x1_common-realcell_x1)/2 ;  i<(realcell_x1_common+realcell_x1)/2; i++)
             for(j=(realcell_y1_common-realcell_y1)/2; j<(realcell_y1_common+realcell_y1)/2; j++)
             { out_common[j+i*realcell_y1_common][0]=out[j-(realcell_y1_common-realcell_y1)/2+(i-(realcell_x1_common-realcell_x1)/2)*realcell_y1][0];
                out_common[j+i*realcell_y1_common][1]=out[j-(realcell_y1_common-realcell_y1)/2+(i-(realcell_x1_common-realcell_x1)/2)*realcell_y1][1];
             }
 	                		
            p=fftwf_plan_dft_2d(realcell_x1_common,realcell_y1_common,out_common, in_common, FFTW_BACKWARD,FFTW_ESTIMATE);
            fftwf_execute(p);
            fftwf_destroy_plan(p); 

 
		  // padding in real space   
		  for(i=0; i<realcell_x1_common; i++)
        	    for(j=0; j<realcell_y1_common; j++)
                 temp_unit[j+i*realcell_x1_common]=in_common[j+i*realcell_x1_common][0]*powf(-1.0,i+j);       
 

           mean=0;  
           for(i=0;i<realcell_x1_common;i++)
             for(j=0;j<realcell_y1_common;j++)
               mean+=temp_unit[j+i*realcell_y1_common];

           mean/=(realcell_x1_common*realcell_y1_common);    
   
	   for(i=0;i<realcell_x;i++)
	        for(j=0;j<realcell_y;j++)   
		     temp_unit_large[j+i*realcell_y]=0.0; // mean;
	     	      
	      for(i=-realcell_x1_common/2;i<realcell_x1_common/2;i++)
	        for(j=-realcell_y1_common/2;j<realcell_y1_common/2;j++)   
			temp_unit_large[j+realcell_y/2+(i+realcell_x/2)*realcell_y]=temp_unit[j+realcell_y1_common/2+(i+realcell_x1_common/2)*realcell_y1_common];
		 
 

		mask(realcell_x1_common,realcell_y1_common,realcell_x,realcell_y,temp_unit_large);

 		for(i=0;i<realcell_x;i++)
	       	 for(j=0;j<realcell_y;j++)   
 			 Image3[j+i*realcell_y+m*realcell_x*realcell_y]=temp_unit_large[j+i*realcell_y];

*/
 
  	
	       m++;   
         } 
      }


	

            
    *num_images=m;
    
 

    free(temp_unit);

  
  
   

}
