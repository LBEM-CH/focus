int flip_image(int nx, int ny, float *image, int flag)
{
     int i,j;
     float *temp_image;
     
     temp_image=(float *)calloc(nx*ny,sizeof(float));
     
     
     if(flag==1)
     	  for(i=0;i<nx;i++)
        	for(j=0;j<ny;j++)
             	   temp_image[j+i*ny]=image[ny-j-1+i*ny];
      else if(flag==2)
      	  for(i=0;i<nx;i++)
                for(j=0;j<ny;j++)
             	   temp_image[j+i*ny]=image[j+(nx-i-1)*ny];
               
      if(flag==1 || flag==2)
      for(i=0;i<nx;i++)
            for(j=0;j<ny;j++)
          	 image[j+i*ny]=temp_image[j+i*ny];
          	 
          	 
      free(temp_image);

}
