/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#ifndef FOURIER_UTILITIES_HPP
#define	FOURIER_UTILITIES_HPP

#include <iostream>
#include <math.h>

#include "../data_structures/common_definitions.hpp"

namespace volume
{
    namespace utilities
    {
        namespace fourier_utilities
        {   
            /**
             * Calculates the resolution of a miller index. 
             * First the size of the reciprocal lattice is calculated, which is
             * then used to calculate the resolution
             * @param index 
             * @param gamma
             * @param a cell length a
             * @param b cell length b
             * @param c cell length c
             * @return resolution
             */
            double GetResolution(const volume::data::MillerIndex& index, 
                    double gamma, double a, double b, double c);
            
        } // namespace fourier_space
        
    } // namespace utilities
    
} // namespace volume_processing_2dx

#endif	/* FOURIER_SPACE_HPP */

