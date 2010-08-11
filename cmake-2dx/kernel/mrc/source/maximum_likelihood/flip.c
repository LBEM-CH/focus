
#include <common.h>


int flip_image(int nx, int ny, float *image, int flag)
{
     int i,j;
     float *temp_image;
     
     temp_image=(float *)calloc(nx*ny,sizeof(float));
     
     
     if(flag==1)
     	  for(i=0;i<nx;i++)
        	for(j=0;j<ny;j++)
             	   temp_image[IDX(i,j,nx,ny)]=image[IDX(i,ny-j-1,nx,ny)];
      else if(flag==2)
      	  for(i=0;i<nx;i++)
                for(j=0;j<ny;j++)
             	   temp_image[IDX(i,j,nx,ny)]=image[IDX((nx-i-1),j,nx,ny)];
               
      if(flag==1 || flag==2)
      for(i=0;i<nx;i++)
            for(j=0;j<ny;j++)
          	 image[IDX(i,j,nx,ny)]=temp_image[IDX(i,j,nx,ny)];
          	 
          	 
      free(temp_image);

}
