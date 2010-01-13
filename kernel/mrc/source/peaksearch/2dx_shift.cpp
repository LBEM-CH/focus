/*--------------------------------------------------------------------------------------------------
    Shift the PS image using each peak spot as the origion and get the average of all shifted images.  
    
    May,2006  by  Xiangyan Zeng  in Stahlberg Lab
    
    Input Parameters:
                sx:   number of rows 
                sy:   number of columns
	       amp:   PS file
	        ML:   number of peak spots
            peak_x:   
            peak_y:   coordinates of peak spots
        peak_value:   PS value of peak spots
    
  
   Output file
       2dx_peaksearch-average.mrc  (average of shifted PS images)
     
 ---------------------------------------------------------------------------------------------------*/    


#include "common.h" 

int shift(int sx, int sy, float *amp, int ML, int *peak_x, int  *peak_y, float *peak_value )


{
 
    float  *new_amp,  *amp_1, min, max,mean, weight;
    int    i,j,new_i,new_j,k, m,  nc,nr;

    int  NR,NS,ISH,m1,i1;
    float temp1[sx],temp2[sx];


    new_amp=(float *)malloc(sizeof(float)*sx*sy);
    amp_1=(float *)malloc(sizeof(float)*sx*sy);
 
  
   
   /*   Shift  Circularly  */
    
    
    for(i=0;i<sx;i++)
       for(j=0;j<sy;j++)
        {  new_amp[j+i*sy]=0;
           amp_1[j+i*sy]=0;
	}
    
    
    mean=0;
    for(i=0;i<sx;i++)
      for(j=0;j<sy;j++)
         mean+=amp[j+i*sy];

    mean=mean/(sx*sy);
    
    
    for(k=0;k<ML;k++)
     {  		    
         
        for(i=0;i<sx;i++)
          for(j=0;j<sy;j++)
            {    new_i=i-(peak_x[k]-sx/2); new_j=j-(peak_y[k]-sy/2);	    
	        
	         if(new_i>=0 && new_i<sx && new_j>=0 && new_j<sy)
                    amp_1[new_j+new_i*sy]=amp[j+i*sy];  
		 else 
		    {
		      while(new_i>=sx)  new_i=new_i-sx;
	              while(new_i<0)    new_i=new_i+sx;
	              while(new_j>=sy)  new_j=new_j-sy;
	              while(new_j<0)    new_j=new_j+sy;    
		      //if(new_i>=sx)  new_i=new_i-sx;
	              //if(new_i<0)    new_i=new_i+sx;
	              //if(new_j>=sy)  new_j=new_j-sy;
	              //if(new_j<0)    new_j=new_j+sy;    
		      amp_1[new_j+new_i*sy]=mean;  
		      }
		      
		   /*   
		   if(new_i<0) new_i=new_i+sx;
                   if(new_i>=sx) new_i=new_i-sx;
		   if(new_j<0) new_j=new_j+sy;
		   if(new_j>=sy) new_j=new_j-sy;	
		   
		   amp_1[new_j+new_i*sy]=amp[j+i*sy];
		   */	   
            }
	    
	  
	    
	for(i=0;i<sx;i++)
	   for(j=0;j<sy;j++)
	      new_amp[j+i*sy]+=amp_1[j+i*sy]*peak_value[k];    
     }
    
    
    
    
    
/*   Mask  the center */    
    
    min=1.0e40;
    for(i=0;i<sx;i++)
       for(j=0;j<sy;j++)
          if(min>new_amp[j+i*sy]) min=new_amp[j+i*sy];
    


   //  printf("min=%f  in the shift \n",min);

    for(i=0;i<sx;i++)
       for(j=0;j<sy;j++)
          if((powf((float)(i-sx/2),2.0)+powf((float)(j-sy/2),2.0))<20*20 || (powf((float)(i-sx/2),2.0)+powf((float)(j-sy/2),2.0))>((sx/2-62*sx/1024)*(sy/2-62*sy/1024)))
	    new_amp[j+i*sy]=min;
	  
    
     
    
/*  Normalize the image  */    

     min=1.0e40; max=-min;
     for(i=0;i<sx; i++)
        for(j=0;j<sy;j++)
         {   if(min>new_amp[j+i*sy]) min=new_amp[j+i*sy];
             if(max<new_amp[j+i*sy]) max=new_amp[j+i*sy];
         }

   //   printf("max=%f min=%f\n",max,min);



/*  write the masked image and the mask  */

   for(i=0;i<sx;i++)
        for(j=0;j<sy;j++)
	  {   new_amp[j+i*sy]=(new_amp[j+i*sy]-min)*1.0/(max-min);
              amp[j+i*sy]=new_amp[j+i*sy];
	      
	      amp_1[i+j*sy]=new_amp[j+i*sy];
	  }
  
    char *complexData = mrcImage::complexFromReal(sx,sy,2,(char*)amp_1);
    mrcImage::mrcHeader *header = mrcImage::headerFromData(sx/2+1,sy,4,complexData);
   
    char fileName[] = "2dx_peaksearch-average.mrc";
    mrcImage(header,complexData,fileName);
    cout<<fileName<<" written"<<endl;


    free(new_amp); 
    free(amp_1);

    return(0);

}
 

