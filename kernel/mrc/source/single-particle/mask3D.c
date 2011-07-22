/*    Mask the particles  */
#ifndef pi
#define pi 3.141592654
#endif

void mask3D(int nx, int ny, int sx, int sy,  float *refer)
  
{    FILE *output;

     
       
     int i,j, m,n,k;
     float mean ;
     float *outdisc, *innerdisc, *annulus, r,r0,f,s,t,max,min,p ;
     
      
     
     outdisc=(float *)malloc(sizeof(float)*sx*sy*sx);
     innerdisc=(float *)malloc(sizeof(float)*sx*sy*sx);
     annulus=(float *)malloc(sizeof(float)*sx*sy*sx);
  

     output=fopen("disc.dat","w");
 

     
     r0=(nx+ny)/4-4;
     p=sqrtf(28.477)*0.1;


     max=-1.0e20; min=-max;
     for(i=0;i<sx;i++)
       for(j=0;j<sy;j++)
         for(n=0;n<sx;n++)
         {  r=sqrtf(powf((float)(i-sx/2),2.0)+powf((float)(j-sy/2),2.0)+powf((float)(n-sx/2),2.0));
            f=p*(r-r0);
            
            s=f/1000;
            t=0.0;
            for(k=0;k<=1000;k++)
               t=t+exp(-powf(k*s,2.0))*s;
            t=2/sqrt(3.14159267)*t;

            outdisc[n+j*sx+i*sy*sx]=0.5*(1-t);
            if(outdisc[n+j*sx+i*sy*sx]>max) max=outdisc[n+j*sx+i*sy*sx];
            if(outdisc[n+j*sx+i*sy*sx]<min) min=outdisc[n+j*sx+i*sy*sx];
 
        }
 
   

      for(i=0;i<sx;i++)
       for(j=0;j<sy;j++)
       for(n=0;n<sx;n++)
        {  outdisc[n+j*sx+i*sy*sx]=(outdisc[n+j*sx+i*sy*sx]-min)/(max-min); 
           if(outdisc[n+j*sx+i*sy*sx]>0.999)  outdisc[n+j*sx+i*sy*sx]=1.0;
           if(outdisc[n+j*sx+i*sy*sx]<0.001)  outdisc[n+j*sx+i*sy*sx]=0.0;  
        }
       


     r0=(nx+ny)/4-10;
     
     max=-1.0e20; min=-max;
    for(i=0;i<sx;i++)
       for(j=0;j<sy;j++)
         for(n=0;n<sx;n++)
         {  r=sqrtf(powf((float)(i-sx/2),2.0)+powf((float)(j-sy/2),2.0)+powf((float)(n-sx/2),2.0));
            f=p*(r-r0);
            
            s=f/1000;
            t=0.0;
            for(k=0;k<=1000;k++)
               t=t+exp(-powf(k*s,2.0))*s;
            t=2/sqrt(3.14159267)*t;

            innerdisc[n+j*sx+i*sx*sy]=1-0.5*(1-t);
            if(innerdisc[n+j*sx+i*sx*sy]>max) max=innerdisc[n+j*sx+i*sx*sy];
            if(innerdisc[n+j*sx+i*sx*sy]<min) min=innerdisc[n+j*sx+i*sx*sy];

          }


        
       for(i=0;i<sx;i++)
         for(j=0;j<sy;j++)
         for(n=0;n<sx;n++)
           { innerdisc[n+j*sx+i*sx*sy]=(innerdisc[n+j*sx+i*sx*sy]-min)/(max-min); 
             if(innerdisc[n+j*sx+i*sx*sy]>0.999)  innerdisc[n+j*sx+i*sx*sy]=1.0;
             if(innerdisc[n+j*sx+i*sx*sy]<0.001)  innerdisc[n+j*sx+i*sx*sy]=0.0;  
           }
 

      float sa=0.0;
      for(i=0;i<sx;i++)
        for(j=0;j<sy;j++)
        for(n=0;n<sx;n++)
         {  annulus[n+j*sx+i*sx*sy]=outdisc[n+j*sx+i*sx*sy]*innerdisc[n+j*sx+i*sx*sy];
          
            sa+=annulus[n+j*sx+i*sx*sy];
         }

 


      mean=0.0;
      for(i=0;i<sx;i++)
        for(j=0;j<sy;j++)
        for(n=0;n<sx;n++)
          mean+=(refer[n+j*sx+i*sx*sy]*annulus[n+j*sx+i*sx*sy]/sa);

 
      
      for(i=0;i<sx;i++)
        for(j=0;j<sy;j++)
        for(n=0;n<sx;n++)
            refer[n+j*sx+i*sx*sy]=refer[n+j*sx+i*sx*sy]*outdisc[n+j*sx+i*sx*sy]+(1-outdisc[n+j*sx+i*sx*sy])*mean;
            
     
      for(i=0;i<sx;i++)
      {  for(j=0;j<sy;j++)
            for(n=0;n<nx;n++)
                fprintf(output,"%f ",outdisc[n+j*sx+i*sx*sy]);
         fprintf(output,"\n");
      }

    fclose(output);
 	    
     free(outdisc);
     free(innerdisc);
     free(annulus);
     
      
       
	    
} 	    
      	    
	
