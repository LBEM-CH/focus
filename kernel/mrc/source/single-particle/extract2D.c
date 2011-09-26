     

void extract2D(float *fobs, float *angles, int  nz, int nx, int ny, float *fgrid)
{      
        int i, j, i1, j1;
       float dm[9], cpsi, ctheta,cphi,spsi,stheta,sphi, xobs,yobs,zobs,dr_rad=3.14159265/180, rad2;
    
       float   boxftv,bsamp1, bsamp2, irrec2=rmax2*rmax2; // nx*ny/4*0.25;
       float *samp, re,im;
       samp=(float *)calloc(2,sizeof(float));
              
       float AMAG=1;
 
       cpsi=cos(angles[0]*dr_rad);
       ctheta=cos(angles[1]*dr_rad);
       cphi=cos(angles[2]*dr_rad);
       spsi=sin(angles[0]*dr_rad);
       stheta=sin(angles[1]*dr_rad);
       sphi=sin(angles[2]*dr_rad);
      	
       	
        dm[0]=(cphi*ctheta*cpsi-sphi*spsi)/AMAG;
        dm[1]=(sphi*ctheta*cpsi+cphi*spsi)/AMAG;
        dm[2]=(-stheta*cpsi)/AMAG;
        dm[3]=(-cphi*ctheta*spsi-sphi*cpsi)/AMAG;
        dm[4]=(-sphi*ctheta*spsi+cphi*cpsi)/AMAG;
        dm[5]=(stheta*spsi)/AMAG;

        dm[6]=stheta*cphi;
        dm[7]=stheta*sphi;
        dm[8]=ctheta;
 
        
        
     //      printf("angle=%f  %f  %f   dm=%f %f %f %f %f %f   \n",angles[0],angles[1], angles[2], dm[0],dm[1],dm[2],dm[3],dm[4],dm[5]);
      
        for(i=0;i<nx;i++)
            for(j=0;j<ny;j++)
            {	  fobs[j+i*ny]=0.0;
              	  fobs[j+i*ny+nx*ny]=0;
	    }	  

 

 
        for(i=-nx/2;i<=nx/2;i++)
           for(j=-nx/2;j<=nx/2;j++)
           {    	i1=i+nx/2;
                	j1=j+ny/2;
            
                 
                 	rad2=i*i+j*j;
 		 	if(rad2<irrec2  )
 		 	{	 		 	
                 		xobs=i*dm[0]+j*dm[3]+nx/2;
             	 		yobs=i*dm[1]+j*dm[4]+ny/2;
             	 		zobs=i*dm[2]+j*dm[5]+nz/2;
       
 		               		     	
 	 	 		if(IRADA==0)       
 		 	      		ainterpo3dbig(xobs, yobs, zobs, nz, nx, ny, fgrid,  samp); 
 				else 
 				       ainterpo3d(xobs, yobs, zobs, nz*IPAD, nx*IPAD, ny*IPAD, fgrid, samp);
 		             
 	
				fobs[j1+i1*ny]+=samp[0]; 
				fobs[j1+i1*ny+nx*ny]+=samp[1];
 		             
 		    	}	
 	       }
 

 
	      for(i=0;i<nx;i++)
		   for(j=0;j<nx;j++)
			if(j>=nx/2)
			{	 
				 i1=nx-i;
				 j1=nx-j;
			         if(i1>=nx) i1=i1-nx;
				 if(j1>=nx) j1=j1-nx;

				 re=(fobs[j+i*ny]+fobs[j1+i1*nx])/2.0;
				 im=(fobs[j+i*ny+nx*ny]-fobs[j1+i1*ny+nx*ny])/2.0;

	     			 fobs[j+i*ny]=re;  
 		        	 fobs[j+i*ny+nx*ny]=im;

				 fobs[j1+i1*ny]=re;  
 		        	 fobs[j1+i1*ny+nx*ny]=-im;

			}
 
  	  
 }
 		
             	 
 
 
 
