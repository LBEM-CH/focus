void  ainterpo3dbig(float  xobs, float yobs, float zobs,  int  sx, int sy, int sz, float *fgrid,  float *samp)
{
       int i, j, i1, j1, L,M,N,LL,MM,NN, nx;
      
       float   ctfv2, boxftv,bsamp1, bsamp2;
       
       nx=sx*IPAD;
     
       	  L=(int)(xobs*IPAD);
 	  M=(int)(yobs*IPAD);
 	  N=(int)(zobs*IPAD);
       
        samp[0]=0.0; 
 	samp[1]=0.0;	
  
  
 	
 	LL=L;
 	MM=M;
 	NN=N; 
 	if(LL>=nx) LL=LL-nx;
 	if(MM>=nx) MM=MM-nx;
 	if(NN>=nx) MM=MM-nx;
 	
 	if(LL<0)   LL=LL+nx;  
 	if(MM<0) MM=MM+nx;
 	if(NN<0) NN=NN+nx;
  
 	          
 	 samp[0]=fgrid[NN+MM*nx+LL*nx*nx];  
         samp[1]=fgrid[NN+MM*nx+LL*nx*nx+nx*nx*nx];
        
  
}
 		                         
