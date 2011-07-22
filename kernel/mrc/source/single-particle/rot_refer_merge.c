/*   Merge the references from all the stacks to generate the common reference for the next ML iteration
*     This dynamical alignment can adjust the nonGaussian spatial relation between the particles from 
*     different crystals
*/


void rot_refer_merge(int Num_angles, int Numstack, int Num_images, float *cry_angle, float *angle, int nx, int ny, float *new_refer, float *refer)
{    int k2, i,j,k;
     float *temp_image, *RTimage, *temp_ref;
     float pow_ref, pow_RT;
     
     
     temp_image=(float *)calloc(nx*ny,sizeof(float));
     temp_ref=(float *)calloc(nx*ny,sizeof(float));
     RTimage=(float *)calloc(nx*ny,sizeof(float));
     
     for(i=0;i<nx;i++)
	  for(j=0;j<ny;j++)
	      refer[j+i*ny]=0;		        

 

     for(i=0;i<nx;i++)
	for(j=0;j<ny;j++)
	  temp_image[j+i*ny]=new_refer[j+i*ny];

     rotate(nx,ny,-(angle[0]-cry_angle[0]),temp_image,temp_ref);
		    
     pow_ref=0.0;

     for(i=0;i<nx;i++)
	for(j=0;j<ny;j++)
	  pow_ref+=powf(temp_ref[j+i*ny],2.0);
 	       


     for(k=0;k<Numstack;k++)	   
 	    for(k2=0;k2<Num_angles;k2++)
  	      {  
		    for(i=0;i<nx;i++)
		       for(j=0;j<ny;j++)
		          temp_image[j+i*ny]=new_refer[j+i*ny+k2*nx*ny+k*nx*ny*Num_angles];
	            
	          //  flip_image(nx,ny,temp_image,flip[k]);
	            
		    rotate(nx,ny,-(angle[k2]-cry_angle[k]),temp_image,RTimage);
		    
		 
		    pow_RT=0.0;
		    
		    for(i=0;i<nx;i++)
		    	for(j=0;j<ny;j++)
		          pow_RT+=powf(RTimage[j+i*ny],2.0);
		     				 
	            for(i=0;i<nx;i++)
		    	for(j=0;j<ny;j++)
		    	  RTimage[j+i*ny]=RTimage[j+i*ny]*sqrt(pow_ref/pow_RT);	
		  
		     
		 
	   
		    for(i=0;i<nx;i++)
		       for(j=0;j<ny;j++)
	   	            refer[j+i*ny]=refer[j+i*ny]+RTimage[j+i*ny];
     	   }
	   
     for(i=0;i<nx;i++)
	  for(j=0;j<ny;j++)
	      refer[j+i*ny]/=Num_images;	   
	   
     free(temp_image);
     free(RTimage);


}
