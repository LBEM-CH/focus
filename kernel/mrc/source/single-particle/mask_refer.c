/*    Mask the particles  */
#define pi 3.1415926
void mask_refer(int nx, int ny, int sx, int sy,  float *refer, float base) 
{   
      	int i,j, P;
      	float mean,devi ;
    	int *M=(int *)calloc(nx*nx,sizeof(int));
 
    	mean=0;
	P=0;				 
	for(i=0;i<nx;i++)
        	for(j=0;j<nx;j++) 
		   	if(powf(i-nx/2,2.0)+powf(j-nx/2,2.0)<rmask*rmask)
                   	{ 	mean+=refer[j+i*nx];  
				P++;  
				M[j+i*nx]=1;
			}
			else 
			{	refer[j+i*nx]=0;
				M[j+i*nx]=0;
			}


	mean/=P;
	devi=0;
	for(i=0;i<nx;i++)
        	for(j=0;j<nx;j++) 
			if(M[j+i*nx]==1)
        		{	refer[j+i*nx]-=mean; 
				devi+=powf(refer[j+i*nx],2.0);
			}

	devi=sqrt(devi/P);

	for(i=0;i<nx;i++)
        	for(j=0;j<nx;j++)
		{ 
		 	 if(M[j+i*nx]==1)
				refer[j+i*nx]=refer[j+i*nx]/devi;
    		}

	free(M);
} 	    
      	    
	
