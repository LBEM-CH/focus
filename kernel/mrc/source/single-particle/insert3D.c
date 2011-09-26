
int insert3D(float *samp, float *angles, int nz, int nx, int ny, float *fgrid1, float *fgrid2, float *num, int *count , float max_max)
{     
       int i, j, k, i1, j1, L,M,N, LL, MM, NN, LS, LT, MS, MT, NS, NT,t;
       float dm[9], arg[3], cpsi, ctheta,cphi,spsi,stheta,sphi, xobs,yobs,zobs,dr_rad=3.14159265/180, rad2,A;
        
       float    L2,M2,N2, ctfv2, ctfv2s2, boxftv,bsamp1, bsamp2, wgt, irrec2=rmax2*rmax2; // nx*ny/4*0.25 ;
       
       
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
   //     dm[6]=stheta*cphi;
  //      dm[7]=stheta*sphi;
   //     dm[8]=ctheta;
 
       
     //  printf("dm=%f %f %f %f %f %f   \n",dm[0],dm[1],dm[2],dm[3],dm[4],dm[5]);
       
       
        for(i=-nx/2;i<nx/2;i++)
           for(j=-ny/2;j<ny/2;j++)
           {     

		 wgt=exp(-(i*i+j*j)/powf(rmax2*0.9,2.0));  // *max_max;

//printf("wgt=%10.4e \n", wgt);

	
                  xobs=i*dm[0]+j*dm[3]+nx/2;
             	   yobs=i*dm[1]+j*dm[4]+ny/2;
             	   zobs=i*dm[2]+j*dm[5]+nz/2;
       
count[(int)zobs+(int)yobs*ny+(int)xobs*ny*nz]+=1;      	  
             	   i1=i+nx/2;
                  j1=j+ny/2;

              	   bsamp1=samp[j1+i1*ny]*wgt;
 		   bsamp2=samp[j1+i1*ny+nx*ny]*wgt;
 
             	 
          	   A=xobs-(float)(IRAD)/2;
                  LS=round(A);
 	  	        if(A<0) LS=LS-1;
 	  	        if(LS<0) LS=0;
 	  	   
 	  	        A=xobs+(float)(IRAD)/2;
 		        LT=round(A);
 		        if(A<0) LT=LT-1;
 		        if(LT>=nx) LT=nx-1;
 		   
 		 
          	   A=yobs-(float)(IRAD)/2;
                  MS=round(A);
 	  	        if(A<0) MS=MS-1;
 	  	        if(MS<0) MS=0;
 	  	   
 	  	        A=yobs+(float)(IRAD)/2;
 		        MT=round(A);
 		        if(A<0) MT=MT-1;
 		        if(MT>=nx) MT=nx-1;
 		   
 		
          	    A=zobs-(float)(IRAD)/2;
                   NS=round(A);
 	  	        if(A<0) NS=NS-1;
 	  	        if(NS<0) NS=0;
 	  	   
 	  	        A=zobs+(float)(IRAD)/2;
 		        NT=round(A);
 		        if(A<0) NT=NT-1;
 		        if(NT>=nx) NT=nx-1;
 	 
 /*
 	 
 		   LS=int(round(xobs));
 	           LT=int(round(xobs));
 	           MS=int(round(yobs));
 	           MT=int(round(yobs));
 	           NS=int(round(zobs));
 	           NT=int(round(zobs));
 */ 	 
 	 	 
 		/* 
 		   LS=int(round(xobs)); //+0.5-(IRAD-1)/2.0));
 		   LT=int(round(xobs)); //;+IRAD/2.0));
 		   MS=int(round(yobs));// +0.5-(IRAD-1)/2.0));
 		   MT=int(round(yobs)); //+IRAD/2.0));
 		   NS=int(round(zobs)); //+0.5-(IRAD-1)/2.0));
 		   NT=int(round(zobs)); //+IRAD/2.0)); 	
*/
 
 	//	ctfv2=powf(ctf[j1+i1*ny],2.0);
 		
 		  for(L=LS;L<=LT;L++)
 		 {  LL=L;
 			L2=powf(L-nx/2,2.0);
 		    	arg[0]=(xobs-L);
 		    	for(M=MS;M<=MT;M++)
 		    	{  	  MM=M;
 		    	       M2=powf(M-nx/2,2.0);
 		        	  arg[1]=(yobs-M);
 		         	  for(N=NS;N<=NT;N++)
 		         	 {     NN=N;
 		         	        N2=powf(N-nx/2,2.0);
 		          	   	arg[2]=(zobs-N);
 		               
 		                  	boxftv=box_ft(arg);
 		                  	rad2=L2+M2+N2;
 		             	   	if(rad2<irrec2 )
 		                  	{	
 		               		       
 		               			if(LL>=nx) LL=LL-nx;
 		               	     		if(MM>nx) MM=MM-ny;
 		               	 		if(NN>nz) NN=NN-nz;
 		               			 
 		               	     		if(LL<0) LL=LL+nx;
 		               			if(MM<0) MM=MM+nx;
 		               			if(NN<0) NN=NN+nx;
 		               		        
 		                        	ctfv2s2=boxftv;
 
  
 		                        	fgrid1[NN+MM*ny+LL*nx*ny]+=bsamp1*boxftv;
 		               			fgrid2[NN+MM*ny+LL*nx*ny]+=bsamp2*boxftv;
 		               		     	
 		               		     	
 		              			num[NN+MM*ny+LL*ny*nz]=num[NN+MM*ny+LL*ny*nz]+boxftv;                       	 
 		                   }
 		             }
 		                      
 		        }
 		    }
 		 
 		    
 		   
          }
          
 }
 		
             	 
 
 
 
 
 
 
 
 
 
 
 
