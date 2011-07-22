/*  Symmertize an image  */
//#define Symmetry 4
void   Symmetrize3D(int nx, int ny, int nz, float *refer)
{
  	int i,j,k2, k;
  	float *temp_image, *RTimage,angle;

      temp_image=(float *)calloc(nx*ny,sizeof(float));
      RTimage=(float *)calloc(nx*ny,sizeof(float));

      for(k=0;k<nz;k++)
      {
         	for(i=0;i<nx;i++)
	  		for(j=0;j<ny;j++)
	       		temp_image[j+i*ny]=refer[k+j*ny+i*nx*ny];	 	  

         for(k2=1;k2<4;k2++)
           {  
	      	angle=(float)(k2)*360/4; 
	      	rotate(nx,ny,angle,temp_image,RTimage); 	      
	
              for(i=0;i<nx;i++)
	         		for(j=0;j<ny;j++)
	           		refer[k+j*ny+i*nx*ny]+=RTimage[j+i*ny];
	    }
      
         for(i=0;i<nx;i++)
	  		for(j=0;j<ny;j++)
	     		refer[k+j*ny+i*nx*ny]/=(4);
       }

       free(temp_image);
       free(RTimage); 


}
