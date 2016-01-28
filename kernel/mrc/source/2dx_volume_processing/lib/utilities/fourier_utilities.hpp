/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#ifndef FOURIER_UTILITIES_HPP
#define	FOURIER_UTILITIES_HPP

#include <iostream>
#include <math.h>

#include "../data_structures/reflection_data.hpp"

namespace tdx
{
    namespace utilities
    {
        namespace fourier_utilities
        {   
            /**
             * Calculates the resolution of a miller index, in Angstroems. 
             * First the size of the reciprocal lattice is calculated, which is
             * then used to calculate the resolution
             * @param index 
             * @param gamma real space angle in radians
             * @param a cell length a, in Angstroems
             * @param b cell length b, in Angstroems
             * @param c cell length c, in Angstroems
             * @return resolution, in Angstroems
             */
            double get_resolution(const tdx::data::MillerIndex& index, 
                    double gamma, double a, double b, double c);
            
             /**
             * Weighted average of the multi-map of miller index and associated peaks.
             * Internally converts a multi-map of miller index to peaks
             * to a map of miller index to individual peak. In the input
             * multi-map, there can exist multiple peaks assigned to
             * one miller index, which will be converted to a map where only
             * one peak is assigned to a miller index.
             * @param peak_multimap map of MillerIndex and it's peaks
             * @param[out] map of MillerIndex and it's peak
             */
            void average_peaks(const tdx::data::MillerToPeakMultiMap& peak_multimap, tdx::data::MillerToPeakMap& peak_map);
            
            /**
             * Calculates phase residual from a given list of peaks. 
             * Calculate phase residual w.r.t the averaged complex.
             * The definition comes from:
             * Marin Van Hell, Ultra Microscopy 21(1978) 95-100, Section 4 
             * @param peaks
             * @return phase residual
             */
            double calculate_phase_residual(const tdx::data::PeakList peaks);
            
        } // namespace fourier_space
        
    } // namespace utilities
    
} // namespace volume_processing_2dx

#endif	/* FOURIER_SPACE_HPP */

