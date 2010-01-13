#include <common.h>

void  ainterpo3d( float  xobs, float yobs, float zobs,  int  nz, int nx, int ny, float *fgrid,   float *samp)
{      
	int i, j, i1, j1, L,M,N, LL,MM,NN, LS, LT, MS, MT, NS, NT;

	float   x, y, z, ctfv2, arg[3], boxftv,bsamp1, bsamp2,A;

	x=IPAD*xobs;
	y=IPAD*yobs;
	z=IPAD*zobs;

	/*
		 LS=int(round(x+0.5))-(IRADA-1) ;
		 LT=int(round(x+0.5))+IRADA;
		 MS=int(round(y+0.5))-(IRADA-1) ;
		 MT=int(round(y+0.5))+IRADA;
		 NS=int(round(z+0.5))-(IRADA-1) ;
		 NT=int(round(z+0.5))+IRADA;
	 */



	LS=int(round(x+0.5-(IRADA-1)/2.0));
	LT=int(round(x+IRADA/2.0));
	MS=int(round(y+0.5-(IRADA-1)/2.0));
	MT=int(round(y+IRADA/2.0));
	NS=int(round(z+0.5-(IRADA-1)/2.0));
	NT=int(round(z+IRADA/2.0)); 	


	/*
		 A=xobs+0.5-IRAD/2;
		 LS=int(round(A));
	//	   if(A<0) LS=LS-1;
	if(LS<-nx/2) LS=-nx/2;

	A=xobs+IRAD/2.0;
	LT=int(round(A));
	//	   if(A<0) LT=LT-1;
#define IRAD 1
if(LT>nx/2) LT=nx/2-1;

A=yobs+0.5-IRAD/2.0;
MS=int(round(A));
	//	   if(A<0) MS=MS-1;
	if(MS<-(nx/2)) MS=-nx/2;

	A=yobs+IRAD/2.0;
	MT=int(round(A));
	//	   if(A<0) MT=MT-1;
	if(MT>nx/2) MT=nx/2-1;

	A=zobs+0.5-IRAD/2.0;
	NS=int(round(A));
	//	   if(A<0) NS=NS-1;
	if(NS<-nx/2) NS=-nx/2;NSAMA

	A=zobs+IRAD/2.0;
	NT=int(round(A));
	//          if(A<0)  NT=NT-1;
	if(NT>nx/2) NT=nx/2-1;

	 */


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
				//	  printf(" &&&&&&&&&  LMN=%d %d %d \n", L,M,N);
				//	  getchar();    	                 

				samp[0]+=fgrid[NN+MM*ny+LL*nx*ny]*boxftv;  
				samp[1]+=fgrid[NN+MM*ny+LL*nx*ny+nx*ny*nz]*boxftv;

			}
		}
	}

	/*
		 for(i=0;i<3;i++)
		 {
		 if(fabs(arg[i])>fabs(dif[i]))  dif[i]=arg[i];
		 if(fabs(arg[i])<fabs(dif[3+i]))  dif[3+i]=arg[i];
		 }*/
}

