#include <stdio.h>
#include <stdlib.h>
 


void simplex(float *SANG,  int nx, float *refer, float *proj, float *ang, int *shift, float *y, int Npar, fftwf_plan p2_fw, fftwf_plan p2_bw)
{	float *par_1, *par_11, *par_cen, *angle_limit1, *angle_limit2;  
	int *shift_1, *shift_11; 
	float y_1, y_11, y_cen, y_l, y_h;
	int i,j,k,l,h;

	float alpha=1, beta=0.5, gama=2;

 	
	par_1=(float *)calloc(3,sizeof(float));	
	par_11=(float *)calloc(3,sizeof(float));
	par_cen=(float *)calloc(3,sizeof(float));
	 
	shift_1=(int *)calloc(2, sizeof(int));
	shift_11=(int *)calloc(2,sizeof(int));
	 
	angle_limit1=(float *)calloc(3,sizeof(float));
	angle_limit2=(float *)calloc(3,sizeof(float));


	for(i=0;i<3;i++)
	{	angle_limit1[i]=*(SANG+i)+max_ang1;
		angle_limit2[i]=*(SANG+i)-max_ang1;
	}

	// get centroid
	for(i=0; i<3; i++)
	{	par_cen[i]=0;
		for(j=0; j<Npar-1; j++)
			par_cen[i]+=ang[i+j*3];

		par_cen[i]/=(Npar-1);
	}
 

	// reflection
	l=Npar-1; 
	h=0;
	y_l=y[l];
	y_h=y[h];

	for(i=0;i<3;i++)
	{	par_1[i]=(1+alpha)*par_cen[i]-alpha*ang[i+l*3];
		if(par_1[i]>angle_limit1[i]) par_1[i]=angle_limit1[i];
		else if(par_1[i]<angle_limit2[i]) par_1[i]=angle_limit2[i];
	} 		

/*
if(k1==0)
printf("11111  max_ang1=%d  par=%f  \n", max_ang1, par_1[0]);
fflush(stdout);
*/
	y_1=ccp(nx, par_1, shift_1, refer,proj, p2_fw, p2_bw);  //calculate maximum cross correlation and return the shift 



	//  expansion
	if(y_1>y_h)
	{	for(i=0;i<3;i++)
		{	par_11[i]=gama*par_1[i]+(1-gama)*par_cen[i];
			if(par_11[i]>angle_limit1[i]) par_11[i]=angle_limit1[i];
			else if(par_11[i]<angle_limit2[i]) par_11[i]=angle_limit2[i];			
		}
 
		y_11=ccp(nx,par_11,shift_11,refer,proj, p2_fw, p2_bw);

		if(y_11>y_h)
		{
			for(i=0;i<3;i++)
				ang[i+h*3]=par_11[i];

			for(i=0;i<2;i++)			
				shift[i+h*2]=shift_11[i];

			y[h]=y_11;
		}
		else
		{	for(i=0;i<3;i++)
				ang[i+h*3]=par_1[i];

			for(i=0;i<2;i++)			
				shift[i+h*2]=shift_1[i];

			y[h]=y_1;
		}
	}
	else		
	{	
		if(y_1>y_l)	 
		{	for(i=0;i<3;i++)
				ang[i+l*3]=par_1[i];
		
			for(i=0;i<2;i++)			
				shift[i+l*2]=shift_1[i];
 		}
		
		for(i=Npar-1; i>0; i--)
		 	if(y_1>y[i])
			{	y[l]=y_1;	
/*
 if(k1==0)
printf("22222  max_ang1=%d  par=%f  \n", max_ang1, ang[0]);
fflush(stdout);
*/

				free(par_1); 	
				free(par_11); 
				free(par_cen); 
	 
				free(shift_1);   
				free(shift_11);  

				return;  		 // p_l replaced by p_1
			}	
	
		//  contraction
		for(i=0;i<3;i++)
		{	par_11[i]=beta*ang[i+l*3]+(1-beta)*par_cen[i];
			if(par_11[i]>angle_limit1[i]) par_11[i]=angle_limit1[i];
			else if(par_11[i]<angle_limit2[i]) par_11[i]=angle_limit2[i];		
		}			

/*
if(k1==0)
printf("33333  max_ang1=%d  par=%f  \n", max_ang1, par_11[0]);
fflush(stdout);
*/
	 	y_11=ccp(nx,par_11, shift_11,refer,proj, p2_fw, p2_bw);

		if(y_11>y_l && y_11>y_1)
		{
			for(i=0;i<3;i++)
				ang[i+l*3]=par_11[i];
			 
			for(i=0;i<2;i++)			
				shift[i+l*2]=shift_11[i];

			y[l]=y_11;
		}
		else
		{	for(j=1;j<Npar;j++)
			{	for(i=0;i<3;i++)
				{	ang[i+j*3]=(ang[i+j*3]+ang[i+h*3])/2;
					par_11[i]=ang[i+j*3];
				}
/*		
if(k1==0)
printf("44444  max_ang1=%d  par=%f  \n", max_ang1, par_11[0]);
fflush(stdout);
*/
			 	y[j]=ccp(nx,par_11,shift_11,refer,proj, p2_fw, p2_bw);

				for(i=0;i<2;i++)			
					shift[i+j*2]=shift_11[i];
			}
		}

	}


        free(par_1); 	
	free(par_11); 
	free(par_cen); 
	 
	free(shift_1);   
	free(shift_11);  

	return;
	
}
	










                             
