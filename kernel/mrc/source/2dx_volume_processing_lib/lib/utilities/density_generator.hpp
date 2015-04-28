/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#ifndef DENSITY_GENERATOR_HPP
#define	DENSITY_GENERATOR_HPP

#include <iostream>
#include <math.h>

#include "../data_structures/real_space_data.hpp"

namespace volume
{
    namespace utilities
    {
        namespace density_generator
        {
            /**
             * Creates an EM density of amount <charge> in a box with size <box_size>^3
             * having a maximum resolution of <max_resolution>. Puts the density in an array
             * of double assuming x to be changing fastest and z the slowest.
             * @param box_size
             * @param max_resolution
             * @param charge
             * @return real space data of size : box_size * box_size * box_size with real space densities in it.
             */
            volume::data::RealSpaceData create_density(int box_size, double max_resolution, double charge); 
            
        }
        
    }
    
}


#endif	/* DENSITY_GENERATOR_HPP */

