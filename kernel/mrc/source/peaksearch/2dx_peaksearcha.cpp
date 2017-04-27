 
/*--------------------------------------------------------------------------------------------------
     Search peaks spots in the original image
    
    May,2006  by  Xiangyan Zeng  in Stahlberg Lab 
    Last update: Nov. 2006 by Xiangyan Zeng
    
    Input Parameters:
                sx:   number of rows 
                sy:   number of columns
	       amp:   PS file
	      r_ML:   number of peak spots
          r_peak_x:   
          r_peak_y:   coordinates of peak spots
      r_peak_value:   PS value of peak spots
   
   
    Defined parameters
       Peak_radius:   minimum distance between candidate peaks     
   
    
       Output file
             peaks_xy_final.dat   (final peak list used for find the lattice)
 
  ---------------------------------------------------------------------------------------------------*/ 

#include "common.h" 

#define peak_radius 6.0

int  peak_search_final(int sx,int sy,float *amp, int r_ML, int *r_peak_x, int *r_peak_y, float *r_peak_value)

{

   
   FILE  *output;
  
   int   i,j, k,m,n, start_x,start_y,ML=r_ML*2;
   int   num_peak, flag,*peaks_image;
   float  A,  max,min,dist, min_dist;
   float cny,cnx,add,cgr;
   int *peak_x,*peak_y;
   float *peak_value;
  
   

   output=fopen("peaks_xy_final.dat","w");
    
   cout<<"peak_search_final:"<<endl;
   cout<<"Number of peak spots (r_ML): "<<r_ML<<endl;
   // cout<<"Malloc for "<<ML<<" int and float variables"<<endl;
 
   //   peaks_image=(int *)malloc(sizeof(int)*sx*sy);
   
   peak_x=(int *)malloc(sizeof(int)*ML);
   peak_y=(int *)malloc(sizeof(int)*ML);
   peak_value=(float *)malloc(sizeof(float)*ML);
   

/*  Peak Search  */
 
    num_peak=0;
    
   for(i=0;i<ML;i++)
   {
     peak_x[i]=0;
     peak_y[i]=0;
     peak_value[i]=0.0;
   } 


    for(i=50;i<sx-50;i++)
      for(j=50;j<sy-50;j++)
        {   A=amp[j+i*sy];    flag=1;


/*  A should be greater than the smallest value in the peak list and its neighbors */   
          if(num_peak>0)
          {
            if(A<peak_value[num_peak-1]) flag=0;
            else
 /*           for(k=i-4; k<=i+4; k++)
              for(m=j-4; m<j+4; m++)
               if(A<amp[m+k*sy]) flag=0;
*/

            if(A<amp[j-1+(i-1)*sy] || 
             A<amp[j+(i-1)*sy] ||
             A<amp[j+1+(i-1)*sy] ||            
             A<amp[j+1+i*sy] ||
	     A<amp[j-1+i*sy] ||
	     A<amp[j-1+(i+1)*sy] ||
             A<amp[j+(i+1)*sy] ||
             A<amp[j+1+(i+1)*sy])  flag=0;
           }





/*  make sure A does not fall into radius of a higher ranking peak */
         
         if(flag==1)
         {  min_dist=1.0e40;
            for(n=0;n<num_peak;n++)
              {  dist=(i-peak_x[n])*(i-peak_x[n])+(j-peak_y[n])*(j-peak_y[n]);
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


/*   Get the center of gravity calculation  
       
       min=100000;
       for(k=i-5;k<=i+5;k++)
          for(m=j-5;m<=j+5;m++)
	      if(amp[m+k*sy]<min) min=amp[m+k*sy];
	   
	   
       cgr=0;  cnx=0;  cny=0; 
       for(k=i-5;k<=i+5;k++)
           for(m=j-5;m<=j+5;m++)
	       if((powf((float)(i-k),2.0)/25+powf((float)(j-m),2.0)/25)<=1)
	           {    add=amp[m+k*sy]-min;
		        cgr=cgr+add;
		        cnx=cnx+(k-i)*add;
			cny=cny+(m-j)*add;
	           }
	
*/




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


/*  find the spot nearest to the center 

     min=1.0e40;
     for(i=0;i<ML;i++)
        if((powf(peak_x[i]-sx/2+1,2.0)+powf(peak_y[i]-sy/2+1,2.0))<min)  
	      {   min=powf(peak_x[i]-sx/2+1,2.0)+powf(peak_y[i]-sy/2+1,2.0);
	          k=i;
	      }
	      
    fprintf(output[0],"%d  \n%10.5f %10.5f %10.5f \n\n",ML,(float)(peak_x[k]-sx/2+1),(float)(-(peak_y[k]-sy/2+1)),peak_value[k]/peak_value[0]);
*/

   
    for(i=0;i<r_ML;i++)
      {       r_peak_x[i]=0;
	      r_peak_y[i]=0;
	      r_peak_value[i]=0.0;
       }


    num_peak=0;
    flag=1;
    for(i=0;i<ML;i++)
    {
      if(num_peak<r_ML && peak_value[i]>0.00001)
      {  
        flag=0;   min=1.0e200;
        for(j=i;j<ML;j++)
          if(fabs((float)(peak_x[i]+peak_x[j]-sx))+ fabs((float)(peak_y[i]+peak_y[j]-sy))<min && i!=j)
	  { min=fabs((float)(peak_x[i]+peak_x[j]-sx))+ fabs((float)(peak_y[i]+peak_y[j]-sy));
	    k=j;
	  }
	 
	  
	 
	//if(min<6)
	//{
            flag=1;
	    r_peak_x[num_peak]=(peak_x[i] - peak_x[k])/2;
	    r_peak_y[num_peak]=(peak_y[i] - peak_y[k])/2;
	    r_peak_value[num_peak]=(peak_value[i]+peak_value[k])/2;
	    
	    num_peak++;

            r_peak_x[num_peak]=-(peak_x[i] - peak_x[k])/2;
	    r_peak_y[num_peak]=-(peak_y[i] - peak_y[k])/2;
	    
	    r_peak_value[num_peak]=(peak_value[i]+peak_value[k])/2;  

            peak_value[i]=0.0;
	    peak_value[k]=0.0;
	    
	
	    num_peak++;
	 //}
 

     }
    }
 
/*  Output the peaks to a file  */

//       for(i=0;i<sx;i++)
//         for(j=0;j<sy;j++)
//             peaks_image[j+i*sy]=0;

      for(i=0;i<num_peak;i++)
         {
	   //            peaks_image[r_peak_y[i]+r_peak_x[i]*sy]=1;

            fprintf(output,"%10.5f %10.5f %10.5f \n",(float)(r_peak_x[i]),(float)((r_peak_y[i])), r_peak_value[i]/r_peak_value[0]);
         } 
  
 
     for(i=0;i<num_peak;i++)
     {
       r_peak_x[i]+= sx/2;
       r_peak_y[i]+= sy/2;
     } 
  
    
       fclose(output); 
       free(peak_x);
       free(peak_y);
       free(peak_value);     


       return(0);
}




