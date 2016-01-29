#include "quality_evaluation.hpp"


double tdx::utilities::quality_evaluation::corrected_phase_residual
                            (const tdx::data::MillerToPeakMultiMap peak_multimap, double a, double b, double c, double gamma, tdx::data::ResolutionBinnedData& binnedPR)
{
    tdx::data::ResolutionBinnedData binned_numerator_sums(binnedPR.min_resolution(), binnedPR.max_resolution(), binnedPR.resolution_bins());
    tdx::data::ResolutionBinnedData binned_denominator_sums(binnedPR.min_resolution(), binnedPR.max_resolution(), binnedPR.resolution_bins());
    
    tdx::data::MillerToPeakMap peak_map;
    tdx::utilities::fourier_utilities::average_peaks(peak_multimap, peak_map);
    
    for(tdx::data::MillerToPeakMultiMap::const_iterator spot_itr=peak_multimap.begin(); spot_itr!=peak_multimap.end(); ++spot_itr)
    {
        //The averaged value
        double averaged_amplitude = peak_map.at((*spot_itr).first).amplitude();
        double averaged_phase = peak_map.at((*spot_itr).first).phase();
        
        //The local value
        double phase = (*spot_itr).second.phase();
        double amplitude = (*spot_itr).second.amplitude();
        
        double phase_diff = tdx::utilities::angle_utilities::CorrectRadianPhase(averaged_phase-phase);
        double resolution = tdx::utilities::fourier_utilities::get_resolution((*spot_itr).first, gamma, a, b, c);

        //Update the sums in the bins
        binned_denominator_sums.add_data_at(1/resolution, std::abs(averaged_amplitude) * std::abs(amplitude));
        binned_numerator_sums.add_data_at(1/resolution, std::abs(averaged_amplitude) * std::abs(amplitude) * pow(phase_diff, 2));
    }
    
    //Divide numerator and denominator in each bin to get final binned phase residuals
    for(int bin=0; bin<binnedPR.resolution_bins(); bin++)
    {
        if(binned_denominator_sums.sum_in(bin) > 0.0000001)
        {
            double bin_phase_residual = sqrt(binned_numerator_sums.sum_in(bin)/binned_denominator_sums.sum_in(bin))*180/M_PI;
            binnedPR.set_bin_sum(bin, bin_phase_residual);
            binnedPR.set_bin_count(bin, 1); //Just to get correct averages
        }
    }
    
}
