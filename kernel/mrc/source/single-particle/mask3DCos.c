/*    Mask the particles  */
#define pi 3.1415926
void mask3D(int nx, int ny, int nz, int sx, int sy, int sz, float *refer) 
{   
      int i,j, m,n,k;
      float mean ;
      float edge, r,r0,r1,f,s,t,max,min,p ;
     
 
     
      r0=(nx+ny+nz)/6-4;
      r1=(nx+ny+nz)/6-8;

     
      t=0.0;
      int num=0;
      for(i=0;i<sx;i++)
       for(j=0;j<sy;j++)
		for(k=0;k<sz; k++)
       	{  	r=sqrtf(powf((float)(i-sx/2),2.0)+powf((float)(j-sy/2),2.0)+powf((float)(k-sz/2),2.0));
           	if(r<=r0 && r>=r1)
            	{	 t+=refer[k+j*sz+i*sy*sz];  num++; }
        	}
     
      t/=num;

printf("IN MASK3D  nx=%d  ny=%d  nz=%d  (nx+nx+nz)/6-4 =%d r0=%f  r1=%f   %f       %d   \n", nx, ny, nz, (nx+nx+nz)/6-4, r0, r1, t, num);
 
      for(i=0;i<sx;i++)
        for(j=0;j<sy;j++)
		for(k=0;k<sz;k++)
         	{  	r=sqrtf(powf((float)(i-sx/2),2.0)+powf((float)(j-sy/2),2.0)+powf((float)(k-sz/2),2.0));
            	if(r<r0 && r>=r1)
             	{ 	 edge=(1.0+cos(pi*(r-r1)/(r0-r1)))/2.0;
               	 refer[k+j*sz+i*sy*sz]=refer[k+j*sz+i*sy*sz]*edge+(1-edge)*t;
             	}
            	else if(r>=r0)
               	 refer[k+j*sz+i*sy*sz]=t;
         }
    
} 	    
      	    
	
