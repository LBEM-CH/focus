/*
 *
 *  Created by Xiangyan Zeng on 1/1/2007
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */

float  quadintp(float xx, float yy, int nx, int ny, float *Image)
{  
    int i,j,IC,JC,HXC,HYC,ip1,im1,jp1,jm1;
    float x,y,dx0,dy0,dxb,dyb;
    float F0, C1,C2,C3,C4,C5;
  
    x=xx;
    y=yy;
    
   
    if(x<0) x=x+(1-(int)(x)/nx)*nx;                   //x=x+nx;
    if(x>=(float)(nx)) x=fmodf(x,(float)(nx));   // x=x-nx;
    if(y<0) y=y+(1-(int)(y)/ny)*ny;                    // y=y+ny;
    if(y>=(float)(ny)) y=fmodf(y,(float)(ny));   // y=y-ny;
    
    i=(int)(x);
    j=(int)(y);
 	 
    dx0=x-i;
    dy0=y-j;
    
    ip1=i+1;
    im1=i-1;
    jp1=j+1;
    jm1=j-1;
  
    if(ip1>=nx) ip1=ip1=ip1-nx;
    if(im1<0)   im1=im1+nx;
    if(jp1>=ny) jp1=jp1-ny;
    if(jp1<0)   jm1=jm1+ny;
    
    F0=Image[j+i*ny];
    C1=Image[j+ip1*ny]-F0;   
    C2=(C1-F0+Image[j+im1*ny])*0.5;
    C3=Image[jp1+i*ny]-F0;
    C4=(C3-F0+Image[jm1+i*ny])*0.5;
    
     if(dx0>=0) HXC=1;
     else HXC=-1;
    
     if(dy0>=0) HYC=1;
     else HYC=-1;
    
     IC=i+HXC;
     JC=j+HYC;  
      
     if(IC>=nx) IC=IC-nx;
     else if(IC<0) IC=IC+nx;     
      
     if(JC>=ny) JC=JC-ny;
     else if(JC<0) JC=JC+ny;
   
   
     dxb=dx0-1;
     dyb=dy0-1;
     
     C5=(Image[JC+IC*ny]-F0-HXC*C1-(HXC*(HXC-1.0))*C2-HYC*C3-(HYC*(HYC-1.0))*C4)*(HXC*HYC);
  	 
      return(F0+dx0*(C1+dxb*C2+dy0*C5)+dy0*(C3+dyb*C4));
	        
    		
}
