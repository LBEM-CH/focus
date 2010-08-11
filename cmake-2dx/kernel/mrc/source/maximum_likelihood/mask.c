/*    Mask the particles  */
#define pi 3.1415926

#include <common.h>

void mask(int nx, int ny, int sx, int sy,  float *refer)

{     
	int i,j, m,n,k;
	float mean ;
	float *outdisc, *innerdisc, *annulus, p,r,r0,f,s,t,max,min ;



	outdisc=(float *)calloc(sx*sy,sizeof(float));
	innerdisc=(float *)calloc(sx*sy,sizeof(float));
	annulus=(float *)calloc(sx*sy,sizeof(float));




	r0=(nx+ny)/4-4;
	p=sqrtf(28.477)*0.2;
	max=-1.0e20; min=-max;
	for(i=0;i<sx;i++)
		for(j=0;j<sy;j++)
		{  r=sqrtf(powf((float)(i-sx/2),2.0)+powf((float)(j-sy/2),2.0));
			f=p*(r-r0);

			s=f/1000;
			t=0.0;
			for(k=0;k<=1000;k++)
				t=t+exp(-powf(k*s,2.0))*s;
			t=2/sqrt(3.14159267)*t;

			outdisc[IDX(i,j,sx,sy)]=0.5*(1-t);
			if(outdisc[IDX(i,j,sx,sy)]>max) max=outdisc[IDX(i,j,sx,sy)];
			if(outdisc[IDX(i,j,sx,sy)]<min) min=outdisc[IDX(i,j,sx,sy)];

		}


	for(i=0;i<sx;i++)
		for(j=0;j<sy;j++)
		{  outdisc[IDX(i,j,sx,sy)]=(outdisc[IDX(i,j,sx,sy)]-min)/(max-min); 
			if(outdisc[IDX(i,j,sx,sy)]>0.99)  outdisc[IDX(i,j,sx,sy)]=1.0;
			if(outdisc[IDX(i,j,sx,sy)]<0.01)  outdisc[IDX(i,j,sx,sy)]=0.0;  
		}



	r0=(nx+ny)/4-10;

	max=-1.0e20; min=-max;
	for(i=0;i<sx;i++)
		for(j=0;j<sy;j++)
		{  r=sqrtf(powf((float)(i-sx/2),2.0)+powf((float)(j-sy/2),2.0));
			f=p*(r-r0);

			s=f/1000;
			t=0.0;
			for(k=0;k<=1000;k++)
				t=t+exp(-powf(k*s,2.0))*s;
			t=2/sqrt(3.14159267)*t;

			innerdisc[IDX(i,j,sx,sy)]=1-0.5*(1-t);
			if(innerdisc[IDX(i,j,sx,sy)]>max) max=innerdisc[IDX(i,j,sx,sy)];
			if(innerdisc[IDX(i,j,sx,sy)]<min) min=innerdisc[IDX(i,j,sx,sy)];

		}



	for(i=0;i<sx;i++)
		for(j=0;j<sy;j++)
		{ innerdisc[IDX(i,j,sx,sy)]=(innerdisc[IDX(i,j,sx,sy)]-min)/(max-min); 
			if(innerdisc[IDX(i,j,sx,sy)]>0.99)  innerdisc[IDX(i,j,sx,sy)]=1.0;
			if(innerdisc[IDX(i,j,sx,sy)]<0.01)  innerdisc[IDX(i,j,sx,sy)]=0.0;  
		}

	float sa=0.0;
	for(i=0;i<sx;i++)
		for(j=0;j<sy;j++)
		{  annulus[IDX(i,j,sx,sy)]=outdisc[IDX(i,j,sx,sy)]*innerdisc[IDX(i,j,sx,sy)];
			sa+=annulus[IDX(i,j,sx,sy)];
		}

	mean=0.0;
	for(i=0;i<sx;i++)
		for(j=0;j<sy;j++)
			mean+=(refer[IDX(i,j,sx,sy)]*annulus[IDX(i,j,sx,sy)]/sa);



	for(i=0;i<sx;i++)
		for(j=0;j<sy;j++)
			refer[IDX(i,j,sx,sy)]=refer[IDX(i,j,sx,sy)]*outdisc[IDX(i,j,sx,sy)]+(1-outdisc[IDX(i,j,sx,sy)])*mean;




	free(outdisc);
	free(innerdisc);
	free(annulus);




} 	    


