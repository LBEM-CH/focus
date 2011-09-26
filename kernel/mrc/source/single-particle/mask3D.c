/*    Mask the particles  */
#ifndef pi
#define pi 3.141592654
#endif

void mask3D(int nx, float *refer)
  
{     int i, j, k,i1,j1,k1, num;

	float m, mean, std, wgt, edge,irad;
	float  *flag=(float *)calloc(nx*nx*nx,sizeof(float)), *refer_filt=(float *)calloc(nx*nx*nx,sizeof(float));
       fftwf_complex *in3,*out3;
       fftwf_plan   p3_fw, p3_bw; 
       
       p3_fw=fftwf_plan_dft_3d(nx,nx,nx, in3,out3,FFTW_FORWARD,FFTW_ESTIMATE);
       p3_bw=fftwf_plan_dft_3d(nx,nx,nx, out3,in3,FFTW_BACKWARD,FFTW_ESTIMATE);
        
       in3=(fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*nx*nx*nx);
       out3=(fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*nx*nx*nx); 
     


    /*  low_pass filtering refer  */  
       for(i=0;i<nx;i++)
	    for(j=0;j<nx;j++)
		for(k=0;k<nx;k++)
		{	in3[k+j*nx+i*nx*nx][0]=refer[k+j*nx+i*nx*nx]*powf(-1.0,i+j+k);
			in3[k+j*nx+i*nx*nx][1]=0;
		}

        fftwf_execute_dft(p3_fw, in3, out3 );
       
 	for(i=0;i<nx;i++)
	    for(j=0;j<nx;j++)
		for(k=0;k<nx;k++)
		{	m=powf(i-nx/2,2.0)+powf(j-nx/2,2.0)+powf(k-nx/2,2.0);
			wgt=exp(-m/(nx*nx*0.01));
     			out3[k+j*nx+i*nx*nx][0]*=wgt;
			out3[k+j*nx+i*nx*nx][1]*=wgt;
		}

	 fftwf_execute_dft(p3_bw, out3, in3 );
  
 
    /*  calculate mean , std */
     
	 mean=0;
	 std=0;

	 for(i=0;i<nx;i++)
	    for(j=0;j<nx;j++)
		for(k=0;k<nx;k++)
		{	
			mean+=in3[k+j*nx+i*nx*nx][0];
			std+=powf(in3[k+j*nx+i*nx*nx][0],2.0);
		}

	 mean/=(nx*nx*nx);
	 std/=(nx*nx*nx);

	 std=std-mean*mean;

	 if(std>0) std=sqrt(std);
	 else std=0;


   /*  thresholding the filtered refer  */
	 float thresh=mean+std;

          for(i=0;i<nx;i++)
	    for(j=0;j<nx;j++)
		for(k=0;k<nx;k++)
		if(in3[k+j*nx+i*nx*nx][0]>thresh) flag[k+j*nx+i*nx*nx]=1.0;
 		else flag[k+j*nx+i*nx*nx]=0.0;	

       
	  for(i=0;i<nx;i++)
	    for(j=0;j<nx;j++)
		for(k=0;k<nx;k++)
		{	if(flag[k+j*nx+i*nx*nx]>0.99)
			for(i1=-5; i1<=5; i1++)
				for(j1=-5; j1<=5; j1++)
					for(k1=-5; k1<=5; k1++)
					{	irad=sqrt((double)i1*i1+(double)j1*j1+(double)k1*k1);	
						edge=(1.0+cos(pi*irad/5.0))/2.0;
						if(i+i1>=0 &&  i+i1<nx && j+j1>=0 && j+j1<nx && k+k1>=0 && k+k1<nx && irad<=5 )
						if(flag[k1+k+(j1+j)*nx+(i1+i)*nx*nx]<edge)  flag[k1+k+(j1+j)*nx+(i1+i)*nx*nx]=edge;
					}
		}


   /*  calculate mean within edge in refer */
	   mean=0;
	   for(i=0;i<nx;i++)
	    for(j=0;j<nx;j++)
		for(k=0;k<nx;k++)	
		if(flag[k+j*nx+i*nx*nx]<1.0 && flag[k+j*nx+i*nx*nx]>1.0e-5)
		{	mean+=refer[k+j*nx+i*nx*nx];
			num++;
		}

	  mean/=num;

	  for(i=0;i<nx;i++)
	    for(j=0;j<nx;j++)
		for(k=0;k<nx;k++)
			refer[k+j*nx+i*nx*nx]=refer[k+j*nx+i*nx*nx]*flag[k+j*nx+i*nx*nx]+(1.0-flag[k+j*nx+i*nx*nx])*mean;
 

	    
} 	    
      	    
	
