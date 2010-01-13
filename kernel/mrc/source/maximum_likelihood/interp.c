/*
 *
 *  Created by Xiangyan Zeng on 1/1/2007
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */

#include <common.h>

void interp(int nx,int ny,int nx_cp, int ny_cp,int num_angles, float *weight, float *weight_cp)
{


	int i,j,k,m,n,l;
	int  *mask;
	float *temp_weight, temp;


	mask=(int *)malloc(sizeof(int)*nx_cp*ny_cp);
	temp_weight=(float *)malloc(sizeof(float)*nx_cp*ny_cp);


	for(k=0;k<num_angles;k++)  
	{

		for(i=0;i<nx_cp;i++)
			for(j=0;j<ny_cp;j++)  
			{ temp_weight[IDX(i,j,nx_cp,ny_cp)]=0.0;
				mask[IDX(i,j,nx_cp,ny_cp)]=0;
			}


		for(i=0;i<nx;i++)
			for(j=0;j<ny;j++) 
			{  temp_weight[IDX(i,j,nx_cp,ny_cp)]=weight[IDX(i,j,nx,ny)+k*nx*ny];
				mask[IDX(i,j,nx_cp,ny_cp)]=1;
			}


		for(i=0;i<nx_cp;i++)
			for(j=0;j<ny_cp;j++) 
				if(mask[IDX(i,j,nx_cp,ny_cp)]==1)
					weight_cp[IDX(i,j,nx_cp,ny_cp)+k*nx_cp*ny_cp]=temp_weight[IDX(i,j,nx_cp,ny_cp)];
				else 
				{  m=0; 
					temp=0.0;
					for(n=-1;n<1;n++)
						for(l=-1;l<1;l++)
							if(n+i>=0 && n+i<nx_cp && j+l>=0 && j+l<ny_cp)
							{  temp+=temp_weight[IDX(i+n,j+l,nx_cp,ny_cp)];
								m++;
							}
					weight_cp[IDX(i,j,nx_cp,ny_cp)+k*nx_cp*ny_cp]=temp/m;

				}

	}

	free(temp_weight);
	free(mask);


}   

