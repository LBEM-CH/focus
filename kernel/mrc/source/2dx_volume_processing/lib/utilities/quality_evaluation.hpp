/* 
 * File:   quality_evaluation.hpp
 * Author: biyanin
 *
 * Created on January 29, 2016, 11:53 AM
 */

#ifndef QUALITY_EVALUATION_HPP
#define	QUALITY_EVALUATION_HPP

#include <iostream>
#include <cmath>

#include "../data_structures/peak_data.hpp"
#include "../data_structures/reflection_data.hpp"
#include "../data_structures/resolution_binned_data.hpp"

#include "fourier_utilities.hpp"
#include "angle_utilities.hpp"

namespace tdx
{
    namespace utilities
    {
        namespace quality_evaluation
        {
            
            /**
             * Calculates corrected phase residual from a given list of peaks. 
             * Provided multimap has multiple peaks associated to each Miller
             * index. The routine first calculates an average from all these
             * multiple peaks and assigns a single peak value to each Miller
             * index. Then using this average calculates the phase residuals 
             * and puts it in the correct bin.
             * The definition of corrected phase residuals comes from:
             * Marin Van Hell, Ultra Microscopy 21(1978) 95-100, Section 4 
             * @param peak_multimap: a map with multiple peaks associated to each Miller index
             * @param a: cell length a
             * @param b: cell length b
             * @param c: cell length c
             * @param gamma: cell angle gamma
             * @param[out] binnedPR: instance where all binned phase residuals are present
             */
            void corrected_phase_residual(const tdx::data::MillerToPeakMultiMap peak_multimap, double a, double b, double c, double gamma, tdx::data::ResolutionBinnedData& binnedPR);
            
            /**
             * Calculates Fourier shell correlation from a given list of peaks. 
             * Provided multimap has multiple peaks associated to each Miller
             * index. The routine first calculates an average from all these
             * multiple peaks and assigns a single peak value to each Miller
             * index. Then using this average calculates the FSC 
             * and puts it in the correct bin.
             * The definition of FSC comes from:
             * Pawel A. Penczek, Methods Enzymol. 2010; 482:73-100.
             * @param peak_multimap: a map with multiple peaks associated to each Miller index
             * @param a: cell length a
             * @param b: cell length b
             * @param c: cell length c
             * @param gamma: cell angle gamma
             * @param[out] binnedFSC: instance where all binned phase residuals are present
             */
            void fourier_shell_correlation(const tdx::data::MillerToPeakMultiMap peak_multimap, double a, double b, double c, double gamma, tdx::data::ResolutionBinnedData& binnedFSC);
            
            
        }
    }
}

#endif	/* QUALITY_EVALUATION_HPP */

