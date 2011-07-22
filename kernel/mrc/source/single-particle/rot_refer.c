void rot_refer(int Num_angles, float *angle, int nx, int ny, float *new_refer, float *refer)
{    int k2, i,j;
     float *temp_image, *RTimage;

     temp_image=(float *)calloc(nx*ny,sizeof(float));
     RTimage=(float *)calloc(nx*ny,sizeof(float));
     
     for(i=0;i<nx;i++)
	  for(j=0;j<ny;j++)
	      refer[j+i*ny]=new_refer[j+i*ny];		        
	   
     for(k2=1;k2<Num_angles;k2++)
        {  
	    for(i=0;i<nx;i++)
	       for(j=0;j<ny;j++)
	          temp_image[j+i*ny]=new_refer[j+i*ny+k2*nx*ny];
	            
	    rotate(nx,ny,-angle[k2],temp_image,RTimage); 
	   
	    for(i=0;i<nx;i++)
	       for(j=0;j<ny;j++)
	          refer[j+i*ny]=refer[j+i*ny]+RTimage[j+i*ny];
        }
	   
     free(temp_image);
     free(RTimage);


}
