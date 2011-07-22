/*    Mask the particles  */
#define pi 3.1415926
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
            
            s=f/100;
            t=0.0;
            for(k=0;k<=100;k++)
               t=t+exp(-powf(k*s,2.0))*s;
            t=2/sqrt(3.14159267)*t;

            outdisc[j+i*sy]=0.5*(1-t);
            if(outdisc[j+i*sy]>max) max=outdisc[j+i*sy];
            if(outdisc[j+i*sy]<min) min=outdisc[j+i*sy];
 
        }
 

      for(i=0;i<sx;i++)
       for(j=0;j<sy;j++)
        {  outdisc[j+i*sy]=(outdisc[j+i*sy]-min)/(max-min); 
           if(outdisc[j+i*sy]>0.99)  outdisc[j+i*sy]=1.0;
           if(outdisc[j+i*sy]<0.01)  outdisc[j+i*sy]=0.0;  
        }



     r0=(nx+ny)/4-10;
     
     max=-1.0e20; min=-max;
     for(i=0;i<sx;i++)
       for(j=0;j<sy;j++)
         {  r=sqrtf(powf((float)(i-sx/2),2.0)+powf((float)(j-sy/2),2.0));
            f=p*(r-r0);
            
            s=f/100;
            t=0.0;
            for(k=0;k<=100;k++)
               t=t+exp(-powf(k*s,2.0))*s;
            t=2/sqrt(3.14159267)*t;

            innerdisc[j+i*sy]=1-0.5*(1-t);
            if(innerdisc[j+i*sy]>max) max=innerdisc[j+i*sy];
            if(innerdisc[j+i*sy]<min) min=innerdisc[j+i*sy];

          }



       for(i=0;i<sx;i++)
         for(j=0;j<sy;j++)
           { innerdisc[j+i*sy]=(innerdisc[j+i*sy]-min)/(max-min); 
             if(innerdisc[j+i*sy]>0.99)  innerdisc[j+i*sy]=1.0;
             if(innerdisc[j+i*sy]<0.01)  innerdisc[j+i*sy]=0.0;  
           }
     
      float sa=0.0;
      for(i=0;i<sx;i++)
        for(j=0;j<sy;j++)
         {  annulus[j+i*sy]=outdisc[j+i*sy]*innerdisc[j+i*sy];
            sa+=annulus[j+i*sy];
         }

      mean=0.0;
      for(i=0;i<sx;i++)
        for(j=0;j<sy;j++)
          mean+=(refer[j+i*sy]*annulus[j+i*sy]/sa);


      
      for(i=0;i<sx;i++)
        for(j=0;j<sy;j++)
            refer[j+i*sy]=refer[j+i*sy]*outdisc[j+i*sy]+(1-outdisc[j+i*sy])*mean;
            
     
     
	    
     free(outdisc);
     free(innerdisc);
     free(annulus);
     
      
       
	    
} 	    
      	    
	
