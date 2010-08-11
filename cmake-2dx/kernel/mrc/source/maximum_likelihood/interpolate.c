/*
 *
 *  Created by Xiangyan Zeng on 1/1/2007
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */

#include <common.h>

void reinterpolat(int nx,int ny,float *real_lat,float *inimage, float *outimage, int outimagelen)
{
	int i,j; 
	float ix,iy;
	float xoffset,yoffset,alength,blength,a1,a2,b1,b2;
	float cornerx[4],cornery[4],centerx,centery,centeroutx,centerouty;

	float xwidth, ywidth;

	a1=real_lat[0];
	a2=real_lat[1];
	b1=real_lat[2];
	b2=real_lat[3];

	xwidth = nx; // a1 + b1;
	if( xwidth > nx ) return;

	ywidth = ny; // a2 + b2;
	if( ywidth > ny ) return;

	cornerx[0]=0;
	cornery[0]=0;
	cornerx[1]=a1;
	cornery[1]=a2;
	cornerx[2]=b1;
	cornery[2]=b2;
	cornerx[3]=a1+b1;
	cornery[3]=a2+b2;

	centerx=0.0;
	centery=0.0;
	for(i=0;i<4;i++)
	{ centerx+=cornerx[i];
		centery+=cornery[i];
	}
	centerx/=4;
	centery/=4;

	xoffset =xwidth/2-centerx;
	yoffset =ywidth/2-centery;


	alength=sqrt(a1*a1+a2*a2);
	blength=sqrt(b1*b1+b2*b2);


	for(i=0;i<outimagelen;i++)
		for (j=0;j<outimagelen;j++)
		{
			ix=( i*a1/outimagelen + j*b1/outimagelen +xoffset);
			iy=( i*a2/outimagelen + j*b2/outimagelen +yoffset);

			outimage[IDX(i,j,outimagelen,outimagelen)] =inimage[IDX((int)ix,(int)iy,nx,ny)]; 

		}


}  






