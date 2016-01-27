/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#ifndef BESSEL_FUNCTIONS_HPP
#define	BESSEL_FUNCTIONS_HPP

#include <math.h>

namespace tdx
{
    namespace utilities
    {
        namespace bessel_functions
        {
            /**
             * Evaluate modified Bessel function In(x) and n=0
             * Uses approximations in precision to calculate the Modified 
             * Bessel function of first kind and 0th order
             * @param value: input value
             * @return value of i0
             */
            double i0(double value);
            
            /**
             * Evaluate modified Bessel function In(x) and n=1
             * Uses approximations in precision to calculate the Modified 
             * Bessel function of first kind and 1st order
             * @param value: input value
             * @return value of i1
             */
            double i1(double value);
            
        } // namespace bessel_functions
        
    } // namespace utilities
    
} // namespace volume_processing_2dx

#endif	/* BESSEL_FUNCTIONS_HPP */

