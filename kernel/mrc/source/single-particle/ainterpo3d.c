 
void  ainterpo3d( float  xobs, float yobs, float zobs,  int  nz, int nx, int ny, float *fgrid,   float *samp)
{      
       int i, j, i1, j1, L,M,N, LL,MM,NN, LS, LT, MS, MT, NS, NT;
      
       float   x, y, z, ctfv2, arg[3], boxftv,bsamp1, bsamp2,A;
 
        x=IPAD*xobs;
        y=IPAD*yobs;
        z=IPAD*zobs;
    
 
           LS=int(x)-IRAD+1;
 	   LT=int(x)+IRAD;
 	   MS=int(y)-IRAD+1 ;
 	   MT=int(y)+IRAD;
 	   NS=int(z)-IRAD+1 ;
 	   NT=int(z)+IRAD;
 
 	   samp[0]=0.0; 
 	   samp[1]=0.0;
 		
 	   for(L=LS;L<=LT;L++)
 	   {  

 		   arg[0]=(x-L)/IPAD;
 		   for(M=MS;M<=MT;M++)
 		   {   	 
 	      	 	  arg[1]=(y-M)/IPAD;
 	       		  for(N=NS;N<=NT;N++)
 	       	 	  {     
 	       			 arg[2]=(z-N)/IPAD;            
 	      			 boxftv=box_ft(arg);          			    
 		             
 		                 LL=L;
 		                 MM=M;
 		                 NN=N;
 		                
                           if(LL>=nx) LL=LL-nx;
                           if(MM>=nx) MM=MM-nx;
                           if(NN>=nx) NN=NN-nx;
 		              
 		                 if(LL<0) LL=L+nx;
 		                 if(MM<0) MM=M+nx;
 		                 if(NN<0) NN=N+nx;
     	                 
 	          
 	    		     	   samp[0]+=fgrid[NN+MM*ny+LL*nx*ny]*boxftv;  
 		                  samp[1]+=fgrid[NN+MM*ny+LL*nx*ny+nx*ny*nz]*boxftv;      
 		            }
 	           }
          }
          
      
}
 		                         
