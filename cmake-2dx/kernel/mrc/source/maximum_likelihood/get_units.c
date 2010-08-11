#define DS 1

#include <common.h>

int get_units(int *num_images,int num_peaks,int tag, float *peak_x, float *peak_y, int sx,int sy,int realcell_x1,int realcell_y1,int realcell_x,int realcell_y,float *unbend_image1, float *unbend_image2, float *Image1, float *Image2)
{   int k,i,j,m;
	double mean,devi_total,devi;
	float *temp_unit;

	temp_unit=(float *)calloc(realcell_x1*realcell_y1,sizeof(float));

	/*  Get the standard deviation of the large image */
	mean=0; devi_total=0.0;
	for(i=0;i<sx;i++)
		for(j=0;j<sy;j++)
			mean+=unbend_image2[IDX(i,j,sx,sy)];

	mean/=(sx*sy);
	for(i=0;i<sx;i++)
		for(j=0;j<sy;j++)
			devi_total+=powf(unbend_image2[IDX(i,j,sx,sy)]-mean,2.0);
	devi_total/=(sx*sy);


	m=0; 
	for(k=tag;k<tag+num_peaks;k++) 
		if(peak_x[k]-realcell_x1/2*DS>=0 && peak_x[k]+realcell_x1/2*DS<sx && peak_y[k]-realcell_y1/2*DS>=0 && peak_y[k]+realcell_y1/2*DS<sy ) 
		{     

			for(i=-realcell_x1/2;i<realcell_x1/2;i++)
				for(j=-realcell_y1/2;j<realcell_y1/2;j++)   
					temp_unit[IDX(i+realcell_x1/2,j+realcell_y1/2,realcell_x1,realcell_y1)]=unbend_image2[IDX(i*DS+(int)peak_x[k],j*DS+(int)peak_y[k],sx,sy)];

			mean=0; devi=0.0;
			for(i=0;i<realcell_x1;i++)
				for(j=0;j<realcell_y1;j++)
					mean+=temp_unit[IDX(i,j,realcell_x1,realcell_y1)];

			mean/=(realcell_x1*realcell_y1);
			for(i=0;i<realcell_x1;i++)
				for(j=0;j<realcell_y1;j++)
					devi+=powf(temp_unit[IDX(i,j,realcell_x1,realcell_y1)]-mean,2.0);
			devi/=(realcell_x1*realcell_y1);

			if(devi>devi_total*0.02)
			{
				for(i=0;i<realcell_x;i++)
					for(j=0;j<realcell_y;j++)   
						Image2[IDX(i,j,realcell_x,realcell_y)+m*realcell_x*realcell_y]=mean;

				for(i=-realcell_x1/2;i<realcell_x1/2;i++)
					for(j=-realcell_y1/2;j<realcell_y1/2;j++)   
						Image2[IDX(i+realcell_x/2,j+realcell_y/2,realcell_x,realcell_y)+m*realcell_x*realcell_y]=temp_unit[IDX(i+realcell_x1/2,j+realcell_y1/2,realcell_x1,realcell_y1)];		  

				for(i=-realcell_x1/2;i<realcell_x1/2;i++)
					for(j=-realcell_y1/2;j<realcell_y1/2;j++)   
						temp_unit[IDX(i+realcell_x1/2,j+realcell_y1/2,realcell_x1,realcell_y1)]=unbend_image1[IDX(i*DS+(int)peak_x[k],j*DS+(int)peak_y[k],sx,sy)];

				mean=0;  
				for(i=0;i<realcell_x1;i++)
					for(j=0;j<realcell_y1;j++)
						mean+=temp_unit[IDX(i,j,realcell_x1,realcell_y1)];

				mean/=(realcell_x1*realcell_y1);       

				for(i=0;i<realcell_x;i++)
					for(j=0;j<realcell_y;j++)   
						Image1[IDX(i,j,realcell_x,realcell_y)+m*realcell_x*realcell_y]=mean;

				for(i=-realcell_x1/2;i<realcell_x1/2;i++)
					for(j=-realcell_y1/2;j<realcell_y1/2;j++)   
						Image1[IDX(i+realcell_x/2,j+realcell_y/2,realcell_x,realcell_y)+m*realcell_x*realcell_y]=temp_unit[IDX(i+realcell_x1/2,j+realcell_y1/2,realcell_x1,realcell_y1)];

				m++;   
			} 
		}

	*num_images=m;



	free(temp_unit);





}
