#include <common.h>

void  ainterpo3dbig(float  xobs, float yobs, float zobs,  int  sx, int sy, int sz, float *fgrid,  float *samp)
{
	int i, j, i1, j1, L,M,N,LL,MM,NN, nx;

	float   ctfv2, boxftv,bsamp1, bsamp2;

	nx=sx*IPAD;


	L=(int(round(xobs))*IPAD);
	M=(int(round(yobs))*IPAD);
	N=(int(round(zobs))*IPAD);


	float x,y,z;

	/*
		 x=xobs*IPAD;
		 y=yobs*IPAD;
		 z=zobs*IPAD;

		 L=int(round(x+(0.5-(IRAD-1)/2.0)));
		 M=int(round(y+(0.5-(IRAD-1)/2.0)));
		 N=int(round(z+(0.5-(IRAD-1)/2.0))); 
	 */


	//   printf("LMN=%d %d %d  \n",L, M, N);
	//    getchar();                 

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


	//	  printf("  BIG   LMN=%d %d %d \n", L,M,N);
	//	  getchar();    

	samp[0]=fgrid[NN+MM*nx+LL*nx*nx];  
	samp[1]=fgrid[NN+MM*nx+LL*nx*nx+nx*nx*nx];

	/*
		 float arg[3];
		 arg[0]=(xobs-L)/IPAD;
		 arg[1]=(yobs-M)/IPAD;
		 arg[2]=(zobs-N)/IPAD;  
		 for(i=0;i<3;i++)
		 {
		 if(fabs(arg[i])>fabs(dif[i]))  dif[i]=arg[i];
		 if(fabs(arg[i])<fabs(dif[3+i]))  dif[3+i]=arg[i];
		 }
	 */
}

