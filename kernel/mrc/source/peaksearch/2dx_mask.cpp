/*--------------------------------------------------------------------------------------------------
   Mask an circle area for peaksearch which excludes the center circle, horizental and vertical strip   
    
   May,2006  by  Xiangyan Zeng  in Stahlberg Lab
    
    
    Input Parameters
                sx:   number of rows
	        sy:   number of columns
	       amp:   PS file
	slit_width:   width of the horizental and vertical strip
	 radius_in:   inner radius of the mask
	radius_out:   outer radius of the mask   
	
    
    
   Output file
          2dx_peaksearch-masked_image.mrc   (masked PS image)
     
 ---------------------------------------------------------------------------------------------------*/    


#include "common.h" 


 int  mask_image(int sx,int sy,float *amp, float slit_width, float radius_in, float radius_out)

{
    
    int    i,j, k,m,n, start_x,start_y;
    int    num_peak, flag,*mask;
    float  *temp_amp, mean, rin,rout,w, A, min, max, dist, min_dist;
   
 
    cout<<" sx = "<<sx<<"     sy = "<<sy<<endl;
    cout<<" slit_width = "<<slit_width<<endl;
    cout<<" radius_in  = "<<radius_in<<endl;
    cout<<" radius_out = "<<radius_out<<endl;

    mask=(int *)malloc(sizeof(int)*sx*sy);
    temp_amp=(float *)malloc(sizeof(float)*sx*sy);
   
   
    rout=radius_out*(sx+sy)/4/0.5;
    rin=radius_in*(sx+sy)/4/0.5;
    w=slit_width*(sx+sy)/4/0.5;
   
   
    mean=0;
    k=0;
    for(i=0;i<sx;i++)
      for(j=0;j<sy;j++)
        {  
           mask[j+i*sy]=1;
	   
	   if((powf(float(i-sx/2),2.0)+powf(float(j-sy/2),2.0))<(rin*rin))
	    {   mean=mean+amp[j+i*sy];  k++;}
        }
	

    mean=mean/k;


/*  Mask the image: Preprocessing to remove the lines, the horizental and vertical  center   */


    
    max=0;
    for(i=sx/2-10;i<sx/2+10;i++)
          for(j=sy/2-10;j<sy/2+10;j++)
            {  if(i!=start_x || j!=start_y)
                 if(amp[j+i*sy]>max) { k=i; m=j; max=amp[j+i*sy];}
            }
         
    cout<<" Max determined."<<endl;

    while(max>mean)
      {   start_x=k; start_y=m;
          while(amp[start_y+start_x*sy]>mean) 
          {  amp[start_y+start_x*sy]=0;
             max=0;
             for(i=start_x-5;i<start_x+5;i++)
                for(j=start_y-5;j<start_y+5;j++)
                   {  if(i!=start_x || j!=start_y)
                       if(amp[j+i*sy]>max) { k=i; m=j; max=amp[j+i*sy];}
                   }
         

            
             for(i=start_x-5;i<start_x+5;i++)
                for(j=start_y-5;j<start_y+5;j++)
                   if(i!=k  ||  j!=m)   
                        {  amp[j+i*sy]=mean;  mask[j+i*sy]=0; }


             start_x=k; start_y=m;



           }
        
          for(i=sx/2-10;i<sx/2+10;i++)
              for(j=sy/2-10;j<sy/2+10;j++)
                {  if(i!=start_x || j!=start_y)
                   if(amp[j+i*sy]>max) { k=i; m=j; max=amp[j+i*sy];}
                 }
		 
		 
       }       
      
     mean=0; m=0;
     for(i=0;i<sx;i++)
       for(j=0;j<sy;j++)
         {  
            if(((i-sx/2)*(i-sx/2)+(j-sy/2)*(j-sy/2))>rout*rout || 
	       ((i-sx/2)*(i-sx/2)+(j-sy/2)*(j-sy/2))<rin*rin ||
	        (abs(i-sx/2)<w) || (abs(j-sy/2)<w)  || mask[j+i*sy]==0)
                  {  mean=mean+amp[j+i*sy]; m++;  mask[j+i*sy]=0; }
         }

     cout<<" binary mask determined."<<endl;

     mean=mean/m; 
     for(i=0;i<sx;i++)
       for(j=0;j<sy;j++)
         {  
            if(mask[j+i*sy]==0)
              amp[j+i*sy]=mean;
         }
     
     cout<<" amp outside of mask set to mean."<<endl;

     min=1.0e40; max=-min;
     for(i=0;i<sx; i++)
       for(j=0;j<sy;j++)
         {
           if(min>amp[j+i*sy]) min=amp[j+i*sy];
           if(max<amp[j+i*sy]) max=amp[j+i*sy];
        }

   // ToDo: Why is this next block done here again??? 

    for(i=0;i<sx;i++)
       for(j=0;j<sy;j++)
          {  mask[j+i*sy]=1;
	     if(((i-sx/2)*(i-sx/2)+(j-sy/2)*(j-sy/2))>rout*rout || 
	       ((i-sx/2)*(i-sx/2)+(j-sy/2)*(j-sy/2))<rin*rin ||
	        (abs(i-sx/2)<w) || (abs(j-sy/2)<w) )  mask[j+i*sy]=0;
//	     if(((i-sx/2+1)*(i-sx/2+1)+(j-sy/2+1)*(j-sy/2+1))>rout*rout || 
//	       ((i-sx/2+1)*(i-sx/2+1)+(j-sy/2+1)*(j-sy/2+1))<rin*rin ||
//	        (abs(i-sx/2+1)<w) || (abs(j-sy/2+1)<w) )  mask[j+i*sy]=0;
           }

/*  write the masked image and the mask  */

     for(i=0;i<sx;i++)
        for(j=0;j<sy;j++)
	  {  amp[j+i*sy]=(amp[j+i*sy]-min)*1.0/(max-min);
 
             temp_amp[i+j*sx]=amp[j+i*sy];
	  }  
      
      
     char *complexData = mrcImage::complexFromReal(sx,sy,2,(char*)temp_amp);
     cout<<"Data converted"<<endl;
     mrcImage::mrcHeader *header = mrcImage::headerFromData(sx/2+1,sy,4,complexData);
   
     cout<<"Header Generated"<<endl;
     char fileName[] = "2dx_peaksearch-masked_image.mrc";
     mrcImage(header,complexData,fileName);
     cout<<fileName<<" written"<<endl;

     


    free(mask);  free(temp_amp);
     

    return 0;

}




