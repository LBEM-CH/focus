 
#include <common.h>

void realspace_lattice(int sx, int sy, float *reci_space, float *real_space)
{   
    float sizex, adjust, astr2, bstr2,astr1,bstr1, pi2=1.570796327;
    float astr, bstr, sinastr, cosastr, sinbstr, cosbstr, singmstr, cosgmstr, gammastr;
    float sina,cosa,sinb,cosb,a,b,aa1,aa2,bb1,bb2,angle;
    
    
    sizex=(float)sx;
    adjust=sizex/sy;
    
    astr1=reci_space[0];
    astr2=reci_space[1];
    bstr1=reci_space[2];
    bstr2=reci_space[3];
    
    astr2=astr2*adjust;
    bstr2=bstr2*adjust;
    
    astr=sqrt(astr1*astr1+astr2*astr2);
    bstr=sqrt(bstr1*bstr1+bstr2*bstr2);
    
    sinastr=astr2/astr;
    cosastr=astr1/astr;
    sinbstr=bstr2/bstr;
    cosbstr=bstr1/bstr;
    
    
    
    singmstr=sinastr*cosbstr-cosastr*sinbstr;
    cosgmstr=sinastr*sinbstr+cosastr*cosbstr;
    gammastr=atan2(singmstr,cosgmstr);
    
    if(gammastr>=0.0)  
       {  aa1=-bstr2;  
          aa2=bstr1;
	  bb1=astr2;
	  bb2=-astr1;
       } 
     else   
       {  aa1=bstr2;  
          aa2=-bstr1;
	  bb1=-astr2;
	  bb2=astr1;
       } 
     
     sina=aa2/bstr;
     cosa=aa1/bstr;
     sinb=bb2/astr;
     cosb=bb1/astr;
     
     if(gammastr<0.0) gammastr=-gammastr;
     a=sx/(astr*sin(gammastr));
     b=sx/(bstr*sin(gammastr));
     
     real_space[0]=a*cosa;
     real_space[1]=a*sina;
     real_space[2]=b*cosb;
     real_space[3]=b*sinb;
     
     
     angle=180*(atan(real_space[3]/real_space[2])-atan(real_space[1]/real_space[0]))/3.1415926;
     if(real_space[2]<0) angle+=180; 
     
     printf("real-space is %f    %f    %f   %f    angle=%f length1=%f length2=%f \n",real_space[0],real_space[1],real_space[2],real_space[3],angle, astr,bstr);
     
     
 }
