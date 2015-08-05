/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#ifndef FOURIERSYMMETRIZATION_HPP
#define	FOURIERSYMMETRIZATION_HPP

#include <math.h>

#include "symmetry2dx.hpp"
#include "../data_structures/common_definitions.hpp"
#include "../data_structures/fourier_space_data.hpp"


namespace volume
{
    namespace symmetrization
    {
        namespace fourier_symmetrization
        {
            /**
             * A Fourier space symmetrization function.
             * Takes in a map with miller indices mapped to the diffraction spot values
             * and changes the map in-place to generate symmetrized map.
             * @param[in/out] fourier_data
             * @param symmetry
             */
            void symmetrize(volume::data::FourierSpaceData& fourier_data, 
                            const volume::symmetrization::Symmetry2dx& symmetry);
            
        }
        
    }
    
}


#endif	/* FOURIERSYMMETRIZATION_HPP */

