

#include "common.h" 

int  peak_search(int sx,int sy,float *amp, int ML, int *peak_x, int *peak_y, float *peak_value)

{

   
   FILE  *output[2];
  
   int   i,j, k,m,n, start_x,start_y;
   int   num_peak, flag,*peaks_image;
   float  A, peak_radius, max,min,dist,min_dist;
   float cny,cnx,add,cgr;
  
   

   output[0]=fopen("peaks_xy.dat","w");
   output[1]=fopen("peaks_image.dat","w");
 
 
   peaks_image=(int *)malloc(sizeof(int)*sx*sy);
   

/*  Peak Search  */
 
    num_peak=0;
    peak_radius=10;
   


    for(i=50;i<sx-50;i++)
      for(j=50;j<sy-50;j++)
        {   A=amp[j+i*sy];    flag=1;


/*  A should be greater than the smallest value in the peak list and its neighbors */   
           if(A<peak_value[num_peak-1]) flag=0;
           else
 /*           for(k=i-4; k<=i+4; k++)
              for(m=j-4; m<j+4; m++)
               if(A<amp[m+k*sy]) flag=0;
*/

           if(A<amp[j-1+(i-1)*sy] || 
            A<amp[j+(i-1)*sy] ||
            A<amp[j+1+(i-1)*sy] ||
            A<amp[j+(i-1)*sy] ||
            A<amp[j+(i+1)*sy] ||
            A<amp[j+1+(i-1)*sy] ||
            A<amp[j+1+i*sy] ||
            A<amp[j+1+(i+1)*sy])  flag=0;
       





/*  make sure A does not fall into radius of a higher ranking peak */
         
         if(flag==1)
         {  min_dist=1.0e40;
            for(n=0;n<num_peak;n++)
              {  dist=(float)((i-peak_x[n])*(i-peak_x[n])+(j-peak_y[n])*(j-peak_y[n]));
	         if(dist<min_dist) 
                  {  min_dist=dist;
                     k=n;
                  }
              }
	    

   
             if(min_dist<peak_radius*peak_radius) 
               {  if(A>peak_value[k]) 
                  {  
		     m=k;
                     while(m>=0 && A>peak_value[m] )  m--;
                     if(m<=k-2)
                        { for(n=k-1;n>=m+1;n--)
                          {  peak_x[n+1]=peak_x[n];
                             peak_y[n+1]=peak_y[n];
                             peak_value[n+1]=peak_value[n];
                          }
                        }  

                      peak_x[m+1]=i;
                      peak_y[m+1]=j;
                      peak_value[m+1]=A;
		   }
                  flag=0;
               }
                 
            }


/*   If A pass the above checks, put it into the peak list  */

        if(flag==1)
             {  
 
                k=0;
                while(A<peak_value[k]) k++;
                if(num_peak>=1)
                 { for(m=ML-2;m>=k;m--)
                       {  peak_x[m+1]=peak_x[m];
                          peak_y[m+1]=peak_y[m];
                          peak_value[m+1]=peak_value[m];
                       }
                  }      
                 peak_x[k]=i;
                 peak_y[k]=j;
                 peak_value[k]=A;
                 num_peak=num_peak+1; 
       
                if(num_peak>ML) num_peak=ML;
             }  
       
  
      }


/*   Get the center of gravity calculation  */
  
    
    for(i=0;i<ML;i++)
    {   
       min=100000;
       for(k=peak_x[i]-5;k<=peak_x[i]+5;k++)
          for(m=peak_y[i]-5;m<=peak_y[i]+5;m++)
	      if(amp[m+k*sy]<min) min=amp[m+k*sy];
	   
	   
       cgr=0;  cnx=0;  cny=0;  
       for(k=peak_x[i]-5;k<=peak_x[i]+5;k++)
          for(m=peak_y[i]-5;m<=peak_y[i]+5;m++)
	       if((powf((float)(peak_x[i]-k),2.0)/25+powf((float)(peak_y[i]-m),2.0)/25)<=1)
	           {    add=amp[m+k*sy]-min;
		        cgr=cgr+add;
		        cnx=cnx+(k-peak_x[i])*add;
			cny=cny+(m-peak_y[i])*add;
			
	           }
	
printf("cnx=%f cny=%f cgr=%f\n",cnx,cny,cgr);

  //     peak_x[i]+=(int)(cnx/cgr);
  //     peak_y[i]+=(int)(cny/cgr);
  //     peak_value[i]=cgr/121;

       fprintf(output[0],"%10.5f %10.5f %10.5f \n",(peak_x[i]+cnx/cgr-sx/2+1),-(peak_y[i]+cny/cgr-sy/2+1), cgr/121);
       
      } 


 
/*  Output the peaks to a file  */

      for(i=0;i<sx;i++)
        for(j=0;j<sy;j++)
            peaks_image[j+i*sy]=0;

      for(i=0;i<ML;i++)
           peaks_image[peak_y[i]+peak_x[i]*sy]=1;
 
  
 
      for(i=0;i<sx;i++)
        for(j=0;j<sy;j++)
           fprintf(output[1],"%d ",peaks_image[j+i*sy]);
      
    
       fclose(output[0]);  fclose(output[1]);
    
       return(0);
       
       
   
}




