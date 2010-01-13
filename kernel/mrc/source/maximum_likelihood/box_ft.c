#ifndef pi
#define pi 3.141592654
#endif

#include <common.h>


float box_ft(float *arg)
{     
  float sincx, prod=1.0;
	int i;

	for(i=0;i<3;i++)
	{  
		if(fabs(arg[i])<1.0e-3)	
			sincx=1.0;
		else    
      sincx=sin(arg[i]*pi)/(arg[i]*pi);

		prod=prod*sincx;
	}


	return(prod);
}



