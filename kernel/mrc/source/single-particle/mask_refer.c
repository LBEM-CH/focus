/*    Mask the particles  */
#define pi 3.1415926
void mask_refer(int nx, int ny, int sx, int sy,  float *refer, float base) 
{   
      int i,j, m,n,k;
      float mean ;
      float edge, r,r0,r1,f,s,t,max,min,p ;
     
 
     
      r0=(nx+ny)/4-4;
      r1=(nx+ny)/4-8;

      
      t=0.0;
      int num=0;
      for(i=0;i<sx;i++)
       for(j=0;j<sy;j++)
       {  	 r=sqrtf(powf((float)(i-sx/2),2.0)+powf((float)(j-sy/2),2.0));
           	 if(r<=r0 && r>=r1)
            	{	 t+=refer[j+i*sy];  num++; }
        }
     
      t/=num;
 
      for(i=0;i<sx;i++)
        for(j=0;j<sy;j++)
         {  	r=sqrtf(powf((float)(i-sx/2),2.0)+powf((float)(j-sy/2),2.0));
            	if(r<r0 && r>=r1)
             	{ 	 edge=(1.0+cos(pi*(r-r1)/(r0-r1)))/2.0;
               		 refer[j+i*sy]=refer[j+i*sy]*edge+(1-edge)*base;
             	}
            	else if(r>=r0)
               	 refer[j+i*sy]=base;
         }
    
} 	    
        
      	    
	
