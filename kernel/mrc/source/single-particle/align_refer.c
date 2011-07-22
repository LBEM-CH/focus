/*    Align the references generated from the current maximum likelihood process on different stacks
       The reference from the first stack is taken as a template, and the remaining references are aligned
       
       Input:  
          new_refer:  ML references
       Output:
          cry_angle:  relative in-plane rotation 
          shx: relative shift in x
          shy: relative shift in y
          flip: indicator of flip   
*/


void align_refer(int Num_angles, int Numstack, int Num_images, float *cry_angle, float *angle, float *shx, float *shy,  int *flip, int nx, int ny, float *new_refer)
{    int k2, i,j,k;
     float *temp_image, *RTimage, *temp_refer, *refer;
     float pow_ref, pow_RT;
     int *pa;
     
     pa=(int *)calloc(4,sizeof(int));
     
     refer=(float *)calloc(nx*ny,sizeof(float));
     temp_image=(float *)calloc(nx*ny,sizeof(float));
     temp_refer=(float *)calloc(nx*ny*Numstack,sizeof(float));
     RTimage=(float *)calloc(nx*ny,sizeof(float));
     
    	        

     /*   Get the references */

     for(k=0;k<Numstack;k++)	
     {   
            for(i=0;i<nx;i++)
	    	for(j=0;j<ny;j++)
	    	    temp_refer[j+i*ny+k*nx*ny]=0;		
 	    for(k2=0;k2<Num_angles;k2++)
  	      {  
		    for(i=0;i<nx;i++)
		       for(j=0;j<ny;j++)
		          temp_image[j+i*ny]=new_refer[j+i*ny+k2*nx*ny+k*nx*ny*Num_angles];
	            
		    rotate(nx,ny,-angle[k2],temp_image,RTimage);
		   
		    for(i=0;i<nx;i++)
		       for(j=0;j<ny;j++)
	   	            temp_refer[j+i*ny+k*nx*ny]=temp_refer[j+i*ny+k*nx*ny]+RTimage[j+i*ny];
     	      }
	   
           for(i=0;i<nx;i++)
	  	for(j=0;j<ny;j++)
	      		temp_refer[j+i*ny+k*nx*ny]/=Num_images;
        }
        
         for(i=0;i<nx;i++)
	  for(j=0;j<ny;j++)
	      refer[j+i*ny]=temp_refer[j+i*ny];	
        
        
       /*  aligna the references */
       for(k=1;k<Numstack;k++)
       {
               for(i=0;i<nx;i++)
		       for(j=0;j<ny;j++)
		          temp_image[j+i*ny]=temp_refer[j+i*ny+k*nx*ny];
               align(nx, ny, refer, temp_image,  pa);
               
               cry_angle[k]=pa[0];
               shx[k]=pa[1];
               shy[k]=pa[2];
               flip[k]=pa[3];
        }  
        	      			   
	   
     free(temp_image);
     free(RTimage);
     free(temp_refer);
     free(refer);
     free(pa);
}
