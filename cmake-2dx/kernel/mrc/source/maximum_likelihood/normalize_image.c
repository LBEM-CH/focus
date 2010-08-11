
#include <common.h>

void normalize_image(int num_images, int realcell_x, int realcell_y, float *Image)
{    int i,j,m;
	float mean,devi;

	for(m=0;m<num_images;m++)
	{	 		  
		mean=0;  devi=0.0;
		for(i=0;i<realcell_x;i++)
			for(j=0;j<realcell_y;j++)
				mean+=Image[IDX(i,j,realcell_x,realcell_y)+m*realcell_x*realcell_y];

		mean=mean/(realcell_x*realcell_y);

		for(i=0;i<realcell_x;i++)
			for(j=0;j<realcell_y;j++)
			{  
				Image[IDX(i,j,realcell_x,realcell_y)+m*realcell_x*realcell_y]=Image[IDX(i,j,realcell_x,realcell_y)+m*realcell_x*realcell_y]-mean;
				devi+=powf(Image[IDX(i,j,realcell_x,realcell_y)+m*realcell_x*realcell_y],2.0);
			}

		devi=sqrt(devi); 
		for(i=0;i<realcell_x;i++)
			for(j=0;j<realcell_y;j++)
				Image[IDX(i,j,realcell_x,realcell_y)+m*realcell_x*realcell_y]/=devi;
	}

}
