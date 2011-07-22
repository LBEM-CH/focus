void normalize_image(int num_images, int realcell_x, int realcell_y, float *Image)
{    int i,j,m;
     float mean,devi;

     for(m=0;m<num_images;m++)
      {	 		  
 	      mean=0;  devi=0.0;
	      for(i=0;i<realcell_x;i++)
	        for(j=0;j<realcell_y;j++)
	          mean+=Image[j+i*realcell_y+m*realcell_x*realcell_y];
			 
	      mean=mean/(realcell_x*realcell_y);
	    
	      for(i=0;i<realcell_x;i++)
	        for(j=0;j<realcell_y;j++)
	        {  
		        Image[j+i*realcell_y+m*realcell_x*realcell_y]=Image[j+i*realcell_y+m*realcell_x*realcell_y]-mean;
		        devi+=powf(Image[j+i*realcell_y+m*realcell_x*realcell_y],2.0);
		 }
	  
	       devi=sqrt(devi/(realcell_x*realcell_y)); 
	       for(i=0;i<realcell_x;i++)
	         for(j=0;j<realcell_y;j++)
	           Image[j+i*realcell_y+m*realcell_x*realcell_y]/=devi;
         }
 
}
