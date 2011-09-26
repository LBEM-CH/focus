/*    Subroutine of 2d-translation by shift_x, shift_y
*/

/*
 *
 *  Created by Xiangyan Zeng on 1/1/2007
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */



void  trans3D(int nx, int shift_x, int shift_y, int shift_z, float *Image)

{   
     float *SFimage;     
     int i,j,k,i_new, j_new,k_new;
      
     SFimage=(float *)calloc(nx*nx*nx,sizeof(float)); 
      
     for(i=0;i<nx*nx*nx;i++)
	  SFimage[i]=0.0;
     
   
     for(i=0;i<nx;i++)
        for(j=0;j<nx;j++)
          for(k=0;k<nx;k++)
	  {   i_new=i+shift_x;
	      j_new=j+shift_y;
              k_new=k+shift_z;
	      if(i_new<0) i_new=i_new+nx;
	      if(i_new>=nx) i_new=i_new-nx;
	      if(j_new<0) j_new=j_new+nx;
	      if(j_new>=nx) j_new=j_new-nx;
              if(k_new<0) k_new=k_new+nx;
	      if(k_new>=nx) k_new=k_new-nx;

	      SFimage[k_new+j_new*nx+i_new*nx*nx]=Image[k+j*nx+i*nx*nx];
         }
       
      for(i=0;i<nx;i++)
        for(j=0;j<nx;j++)
          for(k=0;k<nx;k++)
           Image[k+j*nx+i*nx*nx]=SFimage[k+j*nx+i*nx*nx];

      free(SFimage);
	 
	 
}
	 
