#define DS 1
#define RA 2


int get_units(int *num_images,int num_peaks,int tag, float *peak_x, float *peak_y, int sx,int sy,int realcell_x1,int realcell_y1,int realcell_x,int realcell_y,float *unbend_image1, float *unbend_image2, float *Image1, float *Image2)
{   int k,i,j,m;
     double mean,devi_total,devi;
     float *temp_unit, *temp_unit_large, *temp;
  
    	temp_unit=(float *)calloc(realcell_x1*realcell_y1,sizeof(float));
	temp_unit_large=(float *)calloc(realcell_x*realcell_y,sizeof(float));
	temp=(float *)calloc(realcell_x1*realcell_y1*RA*RA*4,sizeof(float));

  
    /*  Get the standard deviation of the large image */
     mean=0; devi_total=0.0;
     for(i=0;i<sx;i++)
       for(j=0;j<sy;j++)
         mean+=unbend_image2[j+i*sy];

     mean/=(sx*sy);
     for(i=0;i<sx;i++)
       for(j=0;j<sy;j++)
         devi_total+=powf(unbend_image2[j+i*sy]-mean,2.0);
     devi_total/=(sx*sy);


     m=0; 
     for(k=tag;k<tag+num_peaks;k++) 
//     if(peak_x[k]-realcell_x1/2*DS>=0 && peak_x[k]+realcell_x1/2*DS<sx && peak_y[k]-realcell_y1/2*DS>=0 && peak_y[k]+realcell_y1/2*DS<sy ) 
//     {     
 
     if(peak_x[k]-realcell_x1*RA>=0 && peak_x[k]+realcell_x1*RA<sx && peak_y[k]-realcell_y1*RA>=0 && peak_y[k]+realcell_y1*RA<sy ) 
     {  



        	for(i=-realcell_x1/2;i<realcell_x1/2;i++)
	     	for(j=-realcell_y1/2;j<realcell_y1/2;j++)   
		 		temp_unit[j+realcell_y1/2+(i+realcell_x1/2)*realcell_y1]=unbend_image2[j*DS+(int)peak_y[k]+(i*DS+(int)peak_x[k])*sy];
 
 


//	   mask(realcell_x1-4,realcell_y1-4,realcell_x1,realcell_y1,temp_unit);	

        	mean=0; devi=0.0;
        	for(i=0;i<realcell_x1;i++)
          	for(j=0;j<realcell_y1;j++)
             		mean+=temp_unit[j+i*realcell_y1];

        	mean/=(realcell_x1*realcell_y1);
        	for(i=0;i<realcell_x1;i++)
          	for(j=0;j<realcell_y1;j++)
           		devi+=powf(temp_unit[j+i*realcell_y1]-mean,2.0);
        	devi/=(realcell_x1*realcell_y1);
                 
        	if(devi>devi_total*0.02)
        	{	 
       
		 	for(i=0;i<realcell_x;i++)
	        		for(j=0;j<realcell_y;j++)   
		    			temp_unit_large[j+i*realcell_y]=0.0;
 		     	      
	      	for(i=-realcell_x1/2;i<realcell_x1/2;i++)
	        		for(j=-realcell_y1/2;j<realcell_y1/2;j++)   
		     		temp_unit_large[j+realcell_y/2+(i+realcell_x/2)*realcell_y]=temp_unit[j+realcell_y1/2+(i+realcell_x1/2)*realcell_y1];
 

 		 	mask(realcell_x1, realcell_y1, realcell_x, realcell_y, temp_unit_large);

		 	for(i=0;i<realcell_x;i++)
	        		for(j=0;j<realcell_y;j++)   
		    			Image2[j+i*realcell_y+m*realcell_x*realcell_y]=temp_unit_large[j+i*realcell_y];	




/*
           	for(i=-realcell_x1/2;i<realcell_x1/2;i++)
	        		for(j=-realcell_y1/2;j<realcell_y1/2;j++)   
		     		temp_unit[j+realcell_y1/2+(i+realcell_x1/2)*realcell_y1]=unbend_image1[j*DS+(int)peak_y[k]+(i*DS+(int)peak_x[k])*sy];
*/

			for(i=-realcell_x1*RA;i<realcell_x1*RA;i++)
	        		for(j=-realcell_y1*RA;j<realcell_y1*RA;j++)   
		     		temp[j+RA*realcell_y1+(i+RA*realcell_x1)*realcell_y1*RA*2]=-unbend_image1[j*DS+(int)peak_y[k]+(i*DS+(int)peak_x[k])*sy];




mask(realcell_x1*RA*2-4, realcell_y1*RA*2-4, realcell_x1*RA*2, realcell_y1*RA*2, temp);


//if(peak_x[k]>sx/2 && peak_y[k]<sy/2)
//{ printf("In Get Unitf k  peak_x=%f  peak_y=%f   \n", peak_x[k], peak_y[k]);
//   printf("before ctf \n");


//ctf_local(realcell_x1*RA*2,realcell_y1*RA*2, temp, (peak_x[k]-sx/2), -(peak_y[k]-sy/2));



					
 		for(i=-realcell_x1/2;i<realcell_x1/2;i++)
	        for(j=-realcell_y1/2;j<realcell_y1/2;j++)   
		     temp_unit[j+realcell_y1/2+(i+realcell_x1/2)*realcell_y1]= temp[j+realcell_y1*RA+(i+realcell_x1*RA)*realcell_y1*RA*2];


 
		 	for(i=0;i<realcell_x;i++)
	        		for(j=0;j<realcell_y;j++)   
		    			temp_unit_large[j+i*realcell_y]=0.0;
 		     	      
	      	for(i=-realcell_x1/2;i<realcell_x1/2;i++)
	        		for(j=-realcell_y1/2;j<realcell_y1/2;j++)   
		     		temp_unit_large[j+realcell_y/2+(i+realcell_x/2)*realcell_y]=temp_unit[j+realcell_y1/2+(i+realcell_x1/2)*realcell_y1];




 		 	mask(realcell_x1, realcell_y1, realcell_x, realcell_y, temp_unit_large);

		 	for(i=0;i<realcell_x;i++)
	        		for(j=0;j<realcell_y;j++)   
		    			Image1[j+i*realcell_y+m*realcell_x*realcell_y]=temp_unit_large[j+i*realcell_y];


	      	m++;   
         } 
      }
            
    *num_images=m;
    
 

     free(temp_unit);

  
  
   

}
