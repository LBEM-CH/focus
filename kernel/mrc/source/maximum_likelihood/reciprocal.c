/*
 *
 *  Created by Xiangyan Zeng on 1/1/2007
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */

#include <common.h>

void reciprocal(float* real_latt, float* recip_latt, int length)
{
  float a1,a2,b1,b2;
 
  a1=real_latt[0];
  a2=real_latt[1];
  b1=real_latt[2];
  b2=real_latt[3];

  float modUV = a1*b2 - b1*a2;

  recip_latt[0]=b2*length/modUV;
  recip_latt[1]=-b1*length/modUV;
  recip_latt[2]=-a2*length/modUV;
  recip_latt[3]=a1*length/modUV;

  int i;

  printf("real space lattice:  ");
  for(i=0;i<4;i++)
   printf("%f  ",recip_latt[i]);

  printf("\n");

}
