/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include "bessel_functions.hpp"


double volume_processing_2dx::utilities::bessel_functions::i0(double value)
{
    /*
     * SOURCE: http://www.atnf.csiro.au/computing/software/gipsy/sub/bessel.c
     */
   double ax,ans;
   double y;

   if ((ax=fabs(value)) < 3.75) {
      y=value/3.75,y=y*y;
      ans=1.0+y*(3.5156229+y*(3.0899424+y*(1.2067492
         +y*(0.2659732+y*(0.360768e-1+y*0.45813e-2)))));
   } else {
      y=3.75/ax;
      ans=(exp(ax)/sqrt(ax))*(0.39894228+y*(0.1328592e-1
         +y*(0.225319e-2+y*(-0.157565e-2+y*(0.916281e-2
         +y*(-0.2057706e-1+y*(0.2635537e-1+y*(-0.1647633e-1
         +y*0.392377e-2))))))));
   }
   return ans;
}


double volume_processing_2dx::utilities::bessel_functions::i1(double value)
{
   /*
    * SOURCE: http://www.atnf.csiro.au/computing/software/gipsy/sub/bessel.c
    */
   double ax,ans;
   double y;


   if ((ax=fabs(value)) < 3.75) {
      y=value/3.75,y=y*y;
      ans=ax*(0.5+y*(0.87890594+y*(0.51498869+y*(0.15084934
         +y*(0.2658733e-1+y*(0.301532e-2+y*0.32411e-3))))));
   } else {
      y=3.75/ax;
      ans=0.2282967e-1+y*(-0.2895312e-1+y*(0.1787654e-1
         -y*0.420059e-2));
      ans=0.39894228+y*(-0.3988024e-1+y*(-0.362018e-2
         +y*(0.163801e-2+y*(-0.1031555e-1+y*ans))));
      ans *= (exp(ax)/sqrt(ax));
   }
   return value < 0.0 ? -ans : ans;
   
}