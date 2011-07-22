

void HIO1(int nx,  fftwf_complex *fft3, float *refer)
{	int i,j,k;

	 float Beta=0.48, max1=0.0, max2=0.0;

	float E1=0.0, E2=0.0;

	 float *new_refer=(float *)calloc(nx*nx*nx,sizeof(float));
	 fftwf_complex *in3,*out3;
	 fftwf_plan  p3; 
	 in3=(fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*nx*nx*nx);
      out3=(fftwf_complex *)fftwf_malloc(sizeof(fftwf_complex)*nx*nx*nx);     

	

	//  3D FFT of refer
 
	for(i=0;i<nx;i++)
		for(j=0; j<nx; j++)
			for(k=0;k<nx;k++)
			{	in3[k+j*nx+i*nx*nx][0]=refer[k+j*nx+i*nx*nx]*powf(-1.0,i+j+k)/sqrt(nx*nx*nx);
				in3[k+j*nx+i*nx*nx][1]=0;
				E1+=powf(refer[k+j*nx+i*nx*nx],2.0);
			}
 
	 p3=fftwf_plan_dft_3d(nx,nx,nx, in3,out3,FFTW_FORWARD,FFTW_ESTIMATE);
      fftwf_execute(p3);
      fftwf_destroy_plan(p3);
    

	// apply constraints to out3
	for(i=0;i<nx;i++)
	   for(j=0; j<nx; j++)
		for(k=0;k<nx;k++)
		if((powf(fft3[k+j*nx+i*nx*nx][0],2.0)+powf(fft3[k+j*nx+i*nx*nx][1],2.0)>1.0e-4 && powf(i-nx/2,2.0)+powf(j-nx/2,2.0)+powf(k-nx/2,2.0)<rmax2*rmax2*0.49 ) || (powf(i-nx/2,2.0)+powf(j-nx/2,2.0)+powf(k-nx/2,2.0)>rmax2*rmax2*0.49))
			{	
				if(powf(out3[k+j*nx+i*nx*nx][0],2.0)+powf(out3[k+j*nx+i*nx*nx][1],2.0)>max2)
				max2=powf(out3[k+j*nx+i*nx*nx][0],2.0)+powf(out3[k+j*nx+i*nx*nx][1],2.0);

				out3[k+j*nx+i*nx*nx][0]=fft3[k+j*nx+i*nx*nx][0];
				out3[k+j*nx+i*nx*nx][1]=fft3[k+j*nx+i*nx*nx][1];

			}
			else
			{			
				if(powf(out3[k+j*nx+i*nx*nx][0],2.0)+powf(out3[k+j*nx+i*nx*nx][1],2.0)>max1)
					max1=powf(out3[k+j*nx+i*nx*nx][0],2.0)+powf(out3[k+j*nx+i*nx*nx][1],2.0);
			}


		 
 


printf("max1=%f max2=%f \n",max1,max2);



	//  3D inverse FFT of out3 to get new refer,  and apply constraints to refer in real space

	for(i=0;i<nx;i++)
		for(j=0; j<nx; j++)
			for(k=0;k<nx;k++)
			new_refer[k+j*nx+i*nx*nx]=0.0;


      p3=fftwf_plan_dft_3d(nx,nx,nx, out3,in3,FFTW_BACKWARD,FFTW_ESTIMATE);
      fftwf_execute(p3);
      fftwf_destroy_plan(p3);

/*
	for(i=0;i<nx;i++)
         for(j=0;j<nx;j++) 
             for(k=0;k<nx;k++)
             {  	
			if(i>=nx/2-40 && i<=nx/2+40 && j>=nx/2-40 &&k<=nx/2+40 && k>=nx/2-30 && j<=nx/2+30)
				new_refer[k+j*nx+i*nx*nx]=in3[k+j*nx+i*nx*nx][0]*powf(-1.0,i+j+k)/sqrt(nx*nx*nx);	
			else
				new_refer[k+j*nx+i*nx*nx]=refer[k+j*nx+i*nx*nx]-Beta*new_refer[k+j*nx+i*nx*nx];
		   }
*/


	for(i=0;i<nx;i++)
         for(j=0;j<nx;j++) 
             for(k=0;k<nx;k++)
             {  new_refer[k+j*nx+i*nx*nx]=in3[k+j*nx+i*nx*nx][0]*powf(-1.0,i+j+k)/sqrt(nx*nx*nx);		
			if(!(i>=nx/2-40 && i<=nx/2+40 && j>=nx/2-40 && j<=nx/2+40 && k>=nx/2-30 && k<=nx/2+30 && (2*new_refer[k+j*nx+i*nx*nx]-1)>=0 ))
				new_refer[k+j*nx+i*nx*nx]=Beta* refer[k+j*nx+i*nx*nx]-(1-2*Beta)*new_refer[k+j*nx+i*nx*nx];
		   }

		  

	for(i=0;i<nx;i++)
		for(j=0; j<nx; j++)
			for(k=0;k<nx;k++)
			{	refer[k+j*nx+i*nx*nx]=new_refer[k+j*nx+i*nx*nx]; 
				E2+=powf(refer[k+j*nx+i*nx*nx],2.0);
			}

 
	for(i=0;i<nx;i++)
		for(j=0; j<nx; j++)
			for(k=0;k<nx;k++)
			 	refer[k+j*nx+i*nx*nx]=refer[k+j*nx+i*nx*nx]*sqrt(E1/E2); 
		 

	free(new_refer);

	fftwf_free(out3);
     fftwf_free(in3);

}
    

