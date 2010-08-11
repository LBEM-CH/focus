/*    Subroutine of 2d-translation by shift_x, shift_y
 */

/*
 *
 *  Created by Xiangyan Zeng on 1/1/2007
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */

#include <common.h>


void  translate(int nx,int ny,int shift_x,int shift_y,float *Image, float *SFimage)

{   

	int i,j,i_new, j_new;


	for(i=0;i<nx;i++)
		for(j=0;j<ny;j++)
			SFimage[IDX(i,j,nx,ny)]=0.0;


	for(i=0;i<nx;i++)
		for(j=0;j<ny;j++)
		{   i_new=i+shift_x;
			j_new=j+shift_y;
			if(i_new<0) i_new=i_new+nx;
			if(i_new>=nx) i_new=i_new-nx;
			if(j_new<0) j_new=j_new+ny;
			if(j_new>=ny) j_new=j_new-ny;
			SFimage[IDX(i_new,j_new,nx,ny)]=Image[IDX(i,j,nx,ny)];
		}



}

