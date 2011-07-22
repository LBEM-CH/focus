/*  Symmertize an image  */

void   Symmetrize(int nx, int ny, float *refer)
{
  int i,j,k2;
  float *temp_image, *RTimage,angle;

      temp_image=(float *)calloc(nx*ny,sizeof(float));
      RTimage=(float *)calloc(nx*ny,sizeof(float));

   
      for(i=0;i<nx;i++)
	  for(j=0;j<ny;j++)
	       temp_image[j+i*ny]=refer[j+i*ny];	 	  

      for(k2=1;k2<Symmetry;k2++)
           {  
	      angle=(float)(k2)*360/Symmetry; 
	      rotate(nx,ny,angle,temp_image,RTimage); 	      
	
              for(i=0;i<nx;i++)
	         for(j=0;j<ny;j++)
	           refer[j+i*ny]=refer[j+i*ny]+RTimage[j+i*ny];
	    }
      
       for(i=0;i<nx;i++)
	  for(j=0;j<ny;j++)
	     refer[j+i*ny]=refer[j+i*ny]/(Symmetry);


       free(temp_image);
       free(RTimage); 


}
